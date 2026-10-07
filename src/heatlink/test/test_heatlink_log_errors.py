from pathlib import Path
import json, subprocess, tempfile
root = Path(__file__).resolve().parents[1]
rows = []
with tempfile.TemporaryDirectory(prefix='heat-log-errors-') as tmp:
    work = Path(tmp)
    for scenario, text in [('collision', 'already open'), ('bad-path', 'cannot open HEAT_LOG_FILE')]:
        result = subprocess.run([str(root / 'test_heatlink_log'), scenario], cwd=work, capture_output=True, text=True)
        content = result.stdout + result.stderr
        if scenario == 'collision':
            content += (work / 'test_heatlink_log_collision.tmp').read_text()
            assert content.find('CaMa log must be preserved') >= 0
        rows.append({'scenario': scenario, 'returncode': result.returncode, 'pass': result.returncode != 0 and text in content})
    for scenario, config, message in [
        ('empty-name', "&NHEATLINK HEAT_LOG_FILE = '' /\n", 'HEAT_LOG_FILE must not be empty'),
        ('obsolete-log-name', "&NHEATLINK CHEAT_LOG = 'old.log' /\n", 'invalid NHEATLINK'),
        ('invalid-logical', "&NHEATLINK LHEAT_DIAG = 'wrong' /\n", 'invalid NHEATLINK'),
    ]:
        fixture = work / 'config.nml'
        fixture.write_text(config)
        result = subprocess.run([str(root / 'test_heatlink_config'), str(fixture)], cwd=work, capture_output=True, text=True)
        content = result.stdout + result.stderr
        rows.append({'scenario': scenario, 'returncode': result.returncode, 'pass': result.returncode != 0 and message in content})
print(json.dumps(rows, indent=2))
assert all(row['pass'] for row in rows)
