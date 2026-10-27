#include "clients.hpp"
#include "ores.platform/time/datetime.hpp"
#include <array>
#include <charconv>
#include <libpq-fe.h>
#include <memory>
#include <stdexcept>

namespace bench {

namespace {

struct conn_deleter {
    void operator()(PGconn* c) const noexcept {
        PQfinish(c);
    }
};
struct result_deleter {
    void operator()(PGresult* r) const noexcept {
        PQclear(r);
    }
};
using conn_handle = std::unique_ptr<PGconn, conn_deleter>;
using result_handle = std::unique_ptr<PGresult, result_deleter>;

void ensure(PGconn* c, result_handle const& r, const char* what) {
    const auto s = PQresultStatus(r.get());
    if (s != PGRES_COMMAND_OK && s != PGRES_TUPLES_OK && s != PGRES_PIPELINE_SYNC)
        throw std::runtime_error(std::string(what) + ": " + PQerrorMessage(c));
}

constexpr const char* select_columns =
    "select code, tenant_id, version, description, modified_by, performed_by,"
    " change_reason_code, change_commentary, valid_from from bench_crc";

std::vector<row> to_rows(PGresult* r) {
    const int n = PQntuples(r);
    std::vector<row> out;
    out.reserve(n);
    for (int i = 0; i < n; ++i) {
        auto text = [&](int col) {
            return std::string(PQgetvalue(r, i, col), PQgetlength(r, i, col));
        };
        int version = 0;
        const char* v = PQgetvalue(r, i, 2);
        std::from_chars(v, v + PQgetlength(r, i, 2), version);
        out.push_back({text(0),
                       text(1),
                       version,
                       text(3),
                       text(4),
                       text(5),
                       text(6),
                       text(7),
                       ores::platform::time::datetime::from_iso8601_utc(text(8))});
    }
    return out;
}

/*
 * Raw libpq as a thin generated layer would use it: prepared statements, a
 * batch sent in pipeline mode, and point reads one round trip each.
 */
class libpq_client final : public client {
public:
    explicit libpq_client(const credentials& c)
        : conn_(PQconnectdb(c.conninfo().c_str())) {
        if (PQstatus(conn_.get()) != CONNECTION_OK)
            throw std::runtime_error(PQerrorMessage(conn_.get()));
        ensure(conn_.get(), result_handle(PQexec(conn_.get(), setup_sql)), "setup");
        prepare("ins",
                "insert into bench_crc values ($1, $2::uuid, $3::int4, $4, $5, $6, $7, $8,"
                " $9::timestamptz, $10::timestamptz)",
                10);
        prepare("all",
                std::string(select_columns) + " where valid_to = $1::timestamptz order by code",
                1);
        prepare("one",
                std::string(select_columns) + " where code = $1 and valid_to = $2::timestamptz",
                2);
    }

    void reset() override {
        ensure(conn_.get(), result_handle(PQexec(conn_.get(), "truncate bench_crc")), "reset");
    }

    void write_batch(const std::vector<row>& rows) override {
        PGconn* c = conn_.get();
        PQenterPipelineMode(c);
        PQsendQueryParams(c, "begin", 0, nullptr, nullptr, nullptr, nullptr, 0);
        for (const auto& v : rows) {
            const auto version = std::to_string(v.version);
            const std::array<const char*, 10> values{v.code.c_str(),
                                                     v.tenant_id.c_str(),
                                                     version.c_str(),
                                                     v.description.c_str(),
                                                     v.modified_by.c_str(),
                                                     v.performed_by.c_str(),
                                                     v.change_reason_code.c_str(),
                                                     v.change_commentary.c_str(),
                                                     recorded,
                                                     open_end};
            PQsendQueryPrepared(c, "ins", 10, values.data(), nullptr, nullptr, 0);
        }
        PQsendQueryParams(c, "commit", 0, nullptr, nullptr, nullptr, nullptr, 0);
        PQpipelineSync(c);
        for (;;) {
            result_handle r(PQgetResult(c));
            if (!r)
                continue;
            ensure(c, r, "pipeline write");
            if (PQresultStatus(r.get()) == PGRES_PIPELINE_SYNC)
                break;
        }
        PQexitPipelineMode(c);
    }

    std::vector<row> read_latest() override {
        const std::array<const char*, 1> values{open_end};
        result_handle r(PQexecPrepared(conn_.get(), "all", 1, values.data(), nullptr, nullptr, 0));
        ensure(conn_.get(), r, "read latest");
        return to_rows(r.get());
    }

    std::vector<row> read_latest(const std::string& code) override {
        const std::array<const char*, 2> values{code.c_str(), open_end};
        result_handle r(PQexecPrepared(conn_.get(), "one", 2, values.data(), nullptr, nullptr, 0));
        ensure(conn_.get(), r, "read one");
        return to_rows(r.get());
    }

private:
    void prepare(const char* name, const std::string& sql, int n) {
        ensure(conn_.get(),
               result_handle(PQprepare(conn_.get(), name, sql.c_str(), n, nullptr)),
               name);
    }

    conn_handle conn_;
};

}

std::unique_ptr<client> make_libpq_client(const credentials& c) {
    return std::make_unique<libpq_client>(c);
}

}
