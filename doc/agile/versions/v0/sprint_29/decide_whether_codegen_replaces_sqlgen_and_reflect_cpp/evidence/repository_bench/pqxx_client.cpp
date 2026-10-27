#include "clients.hpp"
#include "ores.platform/time/datetime.hpp"
#include <pqxx/pqxx>

namespace bench {

namespace {

constexpr const char* insert_sql =
    "insert into bench_crc values ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)";
constexpr const char* select_columns =
    "select code, tenant_id, version, description, modified_by, performed_by,"
    " change_reason_code, change_commentary, valid_from from bench_crc";

std::vector<row> to_rows(pqxx::result const& r) {
    std::vector<row> out;
    out.reserve(r.size());
    for (auto const& f : r)
        out.push_back({f[0].as<std::string>(),
                       f[1].as<std::string>(),
                       f[2].as<int>(),
                       f[3].as<std::string>(),
                       f[4].as<std::string>(),
                       f[5].as<std::string>(),
                       f[6].as<std::string>(),
                       f[7].as<std::string>(),
                       ores::platform::time::datetime::from_iso8601_utc(f[8].as<std::string>())});
    return out;
}

/*
 * libpqxx as a repository would use it: prepared statements, one round trip a
 * statement, and COPY through stream_to when copy is set.
 */
class pqxx_client final : public client {
public:
    pqxx_client(const credentials& c, bool copy)
        : conn_(c.conninfo())
        , copy_(copy) {
        pqxx::nontransaction{conn_}.exec(setup_sql);
        conn_.prepare("ins", insert_sql);
        conn_.prepare("all", std::string(select_columns) + " where valid_to = $1 order by code");
        conn_.prepare("one", std::string(select_columns) + " where code = $1 and valid_to = $2");
    }

    void reset() override {
        pqxx::nontransaction{conn_}.exec("truncate bench_crc");
    }

    void write_batch(const std::vector<row>& rows) override {
        pqxx::work w{conn_};
        if (copy_) {
            auto s = pqxx::stream_to::table(w,
                                            {"bench_crc"},
                                            {"code",
                                             "tenant_id",
                                             "version",
                                             "description",
                                             "modified_by",
                                             "performed_by",
                                             "change_reason_code",
                                             "change_commentary",
                                             "valid_from",
                                             "valid_to"});
            for (const auto& v : rows)
                s.write_values(v.code,
                               v.tenant_id,
                               v.version,
                               v.description,
                               v.modified_by,
                               v.performed_by,
                               v.change_reason_code,
                               v.change_commentary,
                               recorded,
                               open_end);
            s.complete();
        } else {
            for (const auto& v : rows)
                w.exec(pqxx::prepped{"ins"},
                       pqxx::params{v.code,
                                    v.tenant_id,
                                    v.version,
                                    v.description,
                                    v.modified_by,
                                    v.performed_by,
                                    v.change_reason_code,
                                    v.change_commentary,
                                    recorded,
                                    open_end});
        }
        w.commit();
    }

    std::vector<row> read_latest() override {
        pqxx::nontransaction tx{conn_};
        return to_rows(tx.exec(pqxx::prepped{"all"}, pqxx::params{open_end}));
    }

    std::vector<row> read_latest(const std::string& code) override {
        pqxx::nontransaction tx{conn_};
        return to_rows(tx.exec(pqxx::prepped{"one"}, pqxx::params{code, open_end}));
    }

private:
    pqxx::connection conn_;
    bool copy_;
};

}

std::unique_ptr<client> make_pqxx_client(const credentials& c) {
    return std::make_unique<pqxx_client>(c, false);
}

std::unique_ptr<client> make_pqxx_copy_client(const credentials& c) {
    return std::make_unique<pqxx_client>(c, true);
}

}
