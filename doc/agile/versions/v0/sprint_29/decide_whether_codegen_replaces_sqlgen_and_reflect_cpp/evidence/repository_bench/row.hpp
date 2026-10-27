#ifndef REPOSITORY_BENCH_ROW_HPP
#define REPOSITORY_BENCH_ROW_HPP

#include <chrono>
#include <string>
#include <vector>

namespace bench {

/*
 * The change reason category as the domain sees it. Every client produces and
 * consumes this type, so the comparison measures only the library between the
 * row and the server.
 */
struct row {
    std::string code;
    std::string tenant_id;
    int version = 0;
    std::string description;
    std::string modified_by;
    std::string performed_by;
    std::string change_reason_code;
    std::string change_commentary;
    std::chrono::system_clock::time_point recorded_at;
};

/*
 * The SQL every client runs on its own connection before the workloads: a
 * session table with the columns of ores_dq_change_reason_categories_tbl and
 * none of its triggers, which cost the same whichever client writes.
 */
inline constexpr const char* setup_sql =
    "set time zone 'UTC';"
    "create temp table bench_crc ("
    " code text not null, tenant_id uuid not null, version integer not null,"
    " description text not null, modified_by text not null, performed_by text not null,"
    " change_reason_code text not null, change_commentary text not null,"
    " valid_from timestamptz not null, valid_to timestamptz not null,"
    " primary key (tenant_id, code, valid_from, valid_to));"
    "create index on bench_crc (code, valid_to);";

inline constexpr const char* recorded = "2026-01-01 00:00:00";
inline constexpr const char* open_end = "9999-12-31 23:59:59";

/*
 * One client of the benchmark. Each lives in its own translation unit, so the
 * build also measures what each library costs to compile.
 */
struct client {
    virtual ~client() = default;
    virtual void reset() = 0;
    virtual void write_batch(const std::vector<row>& rows) = 0;
    virtual std::vector<row> read_latest() = 0;
    virtual std::vector<row> read_latest(const std::string& code) = 0;
};

}

#endif
