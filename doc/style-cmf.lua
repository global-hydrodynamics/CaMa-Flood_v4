-- Bidirectional style mapper for pandoc
-- MD -> DOCX: CodeBlock / InlineVar styles
-- DOCX -> MD : CodeBlock style -> fenced code, InlineVar -> inline code

local CODEBLOCK_STYLE  = "CodeBlock"  -- paragraph style in Word
local INLINECODE_STYLE = "InlineVar"  -- character style in Word

-- Convert inlines to text while preserving hard line breaks
local function inlines_to_text(inlines)
  local parts = {}
  for _, il in ipairs(inlines) do
    local t = il.t
    if t == "Str" then
      parts[#parts+1] = il.text
    elseif t == "Space" or t == "SoftBreak" then
      parts[#parts+1] = " "
    elseif t == "LineBreak" then
      parts[#parts+1] = "\n"
    elseif t == "Code" then
      parts[#parts+1] = il.text
    else
      -- fallback for other inline types
      parts[#parts+1] = pandoc.utils.stringify(il)
    end
  end
  return table.concat(parts)
end

-- Convert blocks to text, keeping paragraph boundaries as newlines
local function blocks_to_text(blocks)
  local parts = {}
  for _, b in ipairs(blocks) do
    local t = b.t
    if t == "Para" or t == "Plain" then
      parts[#parts+1] = inlines_to_text(b.content)
    elseif t == "CodeBlock" then
      parts[#parts+1] = b.text
    elseif t == "BlockQuote" then
      parts[#parts+1] = blocks_to_text(b.content)
    else
      parts[#parts+1] = pandoc.utils.stringify(b)
    end
  end
  return table.concat(parts, "\n")
end

-- fenced code block in MD OR CodeBlock-styled paragraph in DOCX
function CodeBlock(el)
  if FORMAT == "docx" then
    -- MD -> DOCX
    -- Put CodeBlock style on a Div so docx writer applies it to contained paragraph
    local div_attr = pandoc.Attr("", {}, {["custom-style"] = CODEBLOCK_STYLE})

    -- preserve line breaks within code block
    local inlines = {}
    local first = true
    for line in el.text:gmatch("([^\n]*)\n?") do
      if line == "" and not first then break end
      if not first then table.insert(inlines, pandoc.LineBreak()) end
      table.insert(inlines, pandoc.Str(line))
      first = false
    end

    return pandoc.Div({ pandoc.Plain(inlines) }, div_attr)
  else
    -- DOCX -> MD
    -- leave CodeBlock here; it will be produced by Div/Para handlers below
    return el
  end
end

-- inline code in MD OR InlineVar-styled span in DOCX
function Code(el)
  if FORMAT == "docx" then
    -- MD -> DOCX
    local attr = pandoc.Attr("", {}, {["custom-style"] = INLINECODE_STYLE})
    return pandoc.Span({ pandoc.Str(el.text) }, attr)
  else
    -- DOCX -> MD
    return el
  end
end

-- DOCX -> MD : CodeBlock style often arrives on Div
function Div(el)
  if FORMAT ~= "docx" and el.attributes
     and el.attributes["custom-style"] == CODEBLOCK_STYLE then
    local txt = blocks_to_text(el.content)
    return pandoc.CodeBlock(txt, pandoc.Attr("", {"code"}, {}))
  end
  return el
end

-- DOCX -> MD : sometimes CodeBlock style lands on Para
function Para(el)
  if FORMAT ~= "docx" and el.attributes
     and el.attributes["custom-style"] == CODEBLOCK_STYLE then
    local txt = pandoc.utils.stringify(el)
    return pandoc.CodeBlock(txt, pandoc.Attr("", {"code"}, {}))
  end
  return el
end

-- DOCX -> MD : InlineVar style arrives on Span
function Span(el)
  if FORMAT ~= "docx" and el.attributes
     and el.attributes["custom-style"] == INLINECODE_STYLE then
    local txt = pandoc.utils.stringify(el)
    return pandoc.Code(txt)
  end
  return el
end

-- DOCX -> MD: sometimes CodeBlock areas are parsed as BlockQuote
-- Convert only "code-ish" blockquotes to fenced code blocks.
function BlockQuote(el)
  if FORMAT == "docx" then
    return el
  end

  local txt = blocks_to_text(el.content)

  -- Heuristic:
  -- treat as code if it has digits AND either
  -- (a) multiple consecutive spaces (tabular/column-like) OR
  -- (b) typical code symbols
  local has_digit = txt:match("%d")
  local has_columns = txt:match("%s%s+")
  local has_code_sym = txt:match("[{}()%[%]=<>:+/*_-]")

  if has_digit and (has_columns or has_code_sym) then
    return pandoc.CodeBlock(txt, pandoc.Attr("", {"code"}, {}))
  end

  return el
end

