import logging
import subprocess
import sys
from datetime import datetime, timedelta

from sqeleton.queries import commit, current_timestamp

from .common import DiffTestCase, CONN_STRINGS
from .test_diff_tables import test_each_database


def run_reladiff_cli(*args):
    try:
        stdout = subprocess.check_output([sys.executable, "-m", "reladiff"] + list(args), stderr=subprocess.PIPE)
    except subprocess.CalledProcessError as e:
        logging.error(e.stderr)
        raise
    return stdout.splitlines()


@test_each_database
class TestCLI(DiffTestCase):
    src_schema = {"id": int, "datetime": datetime, "text_comment": str}

    def setUp(self) -> None:
        super().setUp()

        now = self.connection.query(current_timestamp(), datetime)

        rows = [
            (now, "now"),
            (now - timedelta(seconds=10), "a"),
            (now - timedelta(seconds=7), "b"),
            (now - timedelta(seconds=6), "c"),
        ]

        self.connection.query(
            [
                self.src_table.insert_rows((i, ts, s) for i, (ts, s) in enumerate(rows)),
                self.dst_table.create(self.src_table),
                self.src_table.insert_row(len(rows), now - timedelta(seconds=3), "3 seconds ago"),
                commit,
            ]
        )

    def test_basic(self):
        conn_str = CONN_STRINGS[self.db_cls]
        diff = run_reladiff_cli(conn_str, self.table_src_name, conn_str, self.table_dst_name)
        assert len(diff) == 1

    def test_options(self):
        conn_str = CONN_STRINGS[self.db_cls]
        diff = run_reladiff_cli(
            conn_str,
            self.table_src_name,
            conn_str,
            self.table_dst_name,
            "--bisection-factor",
            "16",
            "--bisection-threshold",
            "10000",
            "--limit",
            "5",
            "-t",
            "datetime",
            "--max-age",
            "1h",
            "--allow-empty-tables"
        )
        assert len(diff) == 1, diff
