#include "ores.platform/environment/environment.hpp"
#include <chrono>
#include <iostream>
#include <pqxx/pqxx>

using clk = std::chrono::steady_clock;

static double ms_since(clk::time_point t) {
    return std::chrono::duration<double, std::milli>(clk::now() - t).count();
}

int main() {
    pqxx::connection c{
        ores::platform::environment::environment::get_value_or_throw("PQXX_PROBE_CONN").c_str()};
    std::cout << "libpqxx " << PQXX_VERSION << ", server " << c.server_version() << "\n";

    pqxx::nontransaction setup{c};
    setup.exec("create temp table probe (id bigint primary key, code text not null, px double "
               "precision, at timestamptz)");
    setup.commit();

    c.prepare("ins", "insert into probe values ($1, $2, $3, now())");
    c.prepare("rd", "select px from probe where id = $1");
    {
        auto t = clk::now();
        pqxx::work w{c};
        for (long i = 0; i < 10000; ++i)
            w.exec(pqxx::prepped{"ins"}, pqxx::params{i, "C" + std::to_string(i), i * 0.5});
        w.commit();
        std::cout << "prepared inserts, 10000 rows one round trip each: " << ms_since(t) << " ms\n";
    }
    {
        auto t = clk::now();
        pqxx::work w{c};
        for (long i = 0; i < 10000; ++i)
            (void)w.exec(pqxx::prepped{"rd"}, pqxx::params{i}).one_field().as<double>();
        w.commit();
        std::cout << "prepared point reads, 10000 one round trip each: " << ms_since(t) << " ms\n";
    }
    {
        auto t = clk::now();
        pqxx::work w{c};
        auto s = pqxx::stream_to::table(w, {"probe"}, {"id", "code", "px"});
        for (long i = 10000; i < 110000; ++i)
            s.write_values(i, "C" + std::to_string(i), i * 0.5);
        s.complete();
        w.commit();
        std::cout << "stream_to COPY, 100000 rows: " << ms_since(t) << " ms\n";
    }
    {
        auto t = clk::now();
        pqxx::work w{c};
        double sum = 0;
        {
            pqxx::pipeline p{w};
            for (long i = 0; i < 10000; ++i)
                p.insert("select px from probe where id = " + std::to_string(i));
            while (!p.empty())
                sum += p.retrieve().second[0][0].as<double>();
        }
        w.commit();
        std::cout << "pqxx::pipeline, 10000 point reads: " << ms_since(t) << " ms (sum " << sum
                  << ")\n";
    }
    {
        auto t = clk::now();
        pqxx::work w{c};
        double sum = 0;
        for (auto [id, code, px] :
             w.query<long, std::string_view, double>("select id, code, px from probe"))
            sum += px;
        w.commit();
        std::cout << "query<> 110000 rows into a tuple: " << ms_since(t) << " ms (sum " << sum
                  << ")\n";
    }
    {
        pqxx::work w{c};
        auto r = w.exec("select 1::int4, 'x'::text, now()");
        std::cout << "result column 0 type oid " << r.column_type(0) << ", text value '"
                  << r[0][0].c_str() << "'\n";
    }
    try {
        pqxx::work w{c};
        w.exec(pqxx::prepped{"ins"}, pqxx::params{1L, "dup", 1.0});
        w.commit();
    } catch (const pqxx::unique_violation& e) {
        std::cout << "duplicate key -> pqxx::unique_violation, sqlstate " << e.sqlstate() << "\n";
    }
    try {
        pqxx::work w{c};
        w.exec("select 1/0");
    } catch (const pqxx::data_exception& e) {
        std::cout << "division by zero -> pqxx::data_exception, sqlstate " << e.sqlstate() << "\n";
    } catch (const pqxx::sql_error& e) {
        std::cout << "division by zero -> pqxx::sql_error, sqlstate " << e.sqlstate() << "\n";
    }
}
