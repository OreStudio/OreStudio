#include <chrono>
#include <cstdio>
#include <cstdlib>
#include <string>
#include <libpq-fe.h>

using clk = std::chrono::steady_clock;

static double ms_since(clk::time_point t) {
    return std::chrono::duration<double, std::milli>(clk::now() - t).count();
}

static void must(PGresult* r, const char* what) {
    auto s = PQresultStatus(r);
    if (s != PGRES_COMMAND_OK && s != PGRES_TUPLES_OK) {
        std::fprintf(stderr, "%s: %s\n", what, PQresultErrorMessage(r));
        std::exit(1);
    }
    PQclear(r);
}

int main() {
    PGconn* c = PQconnectdb(std::getenv("PQXX_PROBE_CONN"));
    must(PQexec(c, "create temp table probe (id bigint primary key, code text not null, px double precision)"), "create");
    must(PQprepare(c, "ins", "insert into probe values ($1::bigint, $2, $3::float8)", 3, nullptr), "prepare ins");
    must(PQprepare(c, "rd", "select px from probe where id = $1::bigint", 1, nullptr), "prepare rd");

    for (const char* stmt : {"ins", "rd"}) {
        auto t = clk::now();
        PQenterPipelineMode(c);
        PQsendQueryParams(c, "begin", 0, nullptr, nullptr, nullptr, nullptr, 0);
        for (long i = 0; i < 10000; ++i) {
            std::string id = std::to_string(i), code = "C" + id, px = std::to_string(i * 0.5);
            const char* vals[] = {id.c_str(), code.c_str(), px.c_str()};
            PQsendQueryPrepared(c, stmt, stmt[0] == 'i' ? 3 : 1, vals, nullptr, nullptr, 0);
        }
        PQsendQueryParams(c, "commit", 0, nullptr, nullptr, nullptr, nullptr, 0);
        PQpipelineSync(c);
        long results = 0;
        for (;;) {
            PGresult* r = PQgetResult(c);
            if (!r) continue;
            auto s = PQresultStatus(r);
            PQclear(r);
            if (s == PGRES_PIPELINE_SYNC) break;
            if (s == PGRES_FATAL_ERROR) { std::fprintf(stderr, "%s\n", PQerrorMessage(c)); return 1; }
            ++results;
        }
        PQexitPipelineMode(c);
        std::printf("libpq pipeline mode, 10000 prepared %s: %.1f ms (%ld results)\n",
                    stmt[0] == 'i' ? "inserts" : "point reads", ms_since(t), results);
    }
    PQfinish(c);
}
