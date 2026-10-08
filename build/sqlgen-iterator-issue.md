## Summary

`sqlgen::postgres::Iterator` opens its own transaction around every read and closes it with `END`, which PostgreSQL treats as `COMMIT`. When a read runs inside a transaction the caller opened, the read commits the caller's transaction.

## Where

`src/sqlgen/postgres/Iterator.cpp`:

```cpp
Iterator::Iterator(const std::string& _sql, const Conn& _conn)
    : cursor_name_(make_cursor_name()), conn_(_conn), end_(false) {
  exec(conn_, "BEGIN").value();
  exec(conn_, "DECLARE " + cursor_name_ + " CURSOR FOR " + _sql).value();
}
...
void Iterator::shutdown() {
  if (!end_) {
    exec(conn_, "CLOSE " + cursor_name_);
    exec(conn_, "END");            // END is COMMIT
    end_ = true;
  }
}
```

## Reproduction

C++20, PostgreSQL 18:

1. Open a transaction: `begin_transaction(conn)`.
2. Run any read, for example `read<std::vector<T>> | where(...)` — this goes through the iterator.
3. Run a write, for example `insert(conn, ...)`.
4. Do not commit. Roll back instead.

The row written in step 3 is committed, because the read in step 2 issued `END`. The `ROLLBACK` reports success and undoes nothing.

## Impact

A unit of work that must write several tables atomically cannot read between the writes. In our case a booking writes an anchor, an activity, a booking and a state. The anchor and the activity derive their write claims by reading first, and each read committed the transaction, so a failure on the later booking insert left the anchor committed. The enclosing `ROLLBACK` did nothing.

## Suggested fix

Ask libpq whether the iterator is already inside a transaction, and touch the transaction only when the iterator opened it:

```cpp
Iterator::Iterator(const std::string& _sql, const Conn& _conn)
    : cursor_name_(make_cursor_name()),
      conn_(_conn),
      end_(false),
      started_transaction_(PQtransactionStatus(conn_.ptr()) == PQTRANS_IDLE) {
  if (started_transaction_)
    exec(conn_, "BEGIN").value();
  exec(conn_, "DECLARE " + cursor_name_ + " CURSOR FOR " + _sql).value();
}

void Iterator::shutdown() {
  if (!end_) {
    exec(conn_, "CLOSE " + cursor_name_);
    if (started_transaction_)
      exec(conn_, "END");
    end_ = true;
  }
}
```

`started_transaction_` must also be carried by the move constructor and the move assignment.

## Version note

The latest tag in this repository is `v0.6.0`, but vcpkg ships `0.8.0#1` built from `47e571149b5e40f63cf7afb5fded134872cc68c0`, and the behaviour above is present at that commit. I have raised the issue against `main`. If the read path has been reworked since, please point me at the right place.
