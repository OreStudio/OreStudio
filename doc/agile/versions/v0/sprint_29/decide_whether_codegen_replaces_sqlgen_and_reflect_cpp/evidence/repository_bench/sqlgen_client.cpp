#include "clients.hpp"
#include "ores.platform/time/datetime.hpp"
#include <sqlgen.hpp>
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace bench {

namespace {

using db_timestamp = sqlgen::Timestamp<"%Y-%m-%d %H:%M:%S">;

struct crc_entity {
    constexpr static const char* schema = "pg_temp";
    constexpr static const char* tablename = "bench_crc";

    sqlgen::PrimaryKey<std::string> code;
    std::string tenant_id;
    int version = 0;
    std::string description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    db_timestamp valid_from = open_end;
    db_timestamp valid_to = open_end;
};

template <typename T>
T ensure(sqlgen::Result<T> r) {
    if (!r)
        throw std::runtime_error(r.error().what());
    return std::move(*r);
}

std::vector<row> to_rows(const std::vector<crc_entity>& entities) {
    std::vector<row> r;
    r.reserve(entities.size());
    for (const auto& e : entities)
        r.push_back({e.code.value(), e.tenant_id, e.version, e.description, e.modified_by,
                     e.performed_by, e.change_reason_code, e.change_commentary,
                     ores::platform::time::datetime::from_iso8601_utc(e.valid_from.str() + "+00")});
    return r;
}

class sqlgen_client final : public client {
public:
    explicit sqlgen_client(const sqlgen::postgres::Credentials& c)
        : conn_(ensure(sqlgen::postgres::connect(c))) {
        ensure(conn_->execute(setup_sql));
    }

    void reset() override { ensure(conn_->execute("truncate bench_crc")); }

    void write_batch(const std::vector<row>& rows) override {
        std::vector<crc_entity> entities;
        entities.reserve(rows.size());
        for (const auto& v : rows)
            entities.push_back({v.code, v.tenant_id, v.version, v.description, v.modified_by,
                                v.performed_by, v.change_reason_code, v.change_commentary,
                                recorded, open_end});
        ensure(sqlgen::Result<sqlgen::Ref<sqlgen::postgres::Connection>>(conn_)
                   .and_then(sqlgen::begin_transaction)
                   .and_then(sqlgen::insert(entities))
                   .and_then(sqlgen::commit));
    }

    std::vector<row> read_latest() override {
        using namespace sqlgen::literals;
        static const db_timestamp max = open_end;
        const auto query = sqlgen::read<std::vector<crc_entity>> |
                           sqlgen::where("valid_to"_c == max) | sqlgen::order_by("code"_c);
        return to_rows(ensure(query(conn_)));
    }

    std::vector<row> read_latest(const std::string& code) override {
        using namespace sqlgen::literals;
        static const db_timestamp max = open_end;
        const auto query = sqlgen::read<std::vector<crc_entity>> |
                           sqlgen::where("code"_c == code && "valid_to"_c == max);
        return to_rows(ensure(query(conn_)));
    }

private:
    sqlgen::Ref<sqlgen::postgres::Connection> conn_;
};

}

std::unique_ptr<client> make_sqlgen_client(const credentials& c) {
    return std::make_unique<sqlgen_client>(sqlgen::postgres::Credentials{
        .user = c.user, .password = c.password, .host = c.host, .dbname = c.dbname});
}

}
