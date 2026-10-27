#ifndef REPOSITORY_BENCH_CLIENTS_HPP
#define REPOSITORY_BENCH_CLIENTS_HPP

#include "row.hpp"
#include <memory>
#include <string>

namespace bench {

struct credentials {
    std::string host;
    std::string dbname;
    std::string user;
    std::string password;

    std::string conninfo() const {
        return "host=" + host + " dbname=" + dbname + " user=" + user + " password=" + password;
    }
};

std::unique_ptr<client> make_sqlgen_client(const credentials& c);
std::unique_ptr<client> make_pqxx_client(const credentials& c);
std::unique_ptr<client> make_pqxx_copy_client(const credentials& c);
std::unique_ptr<client> make_libpq_client(const credentials& c);

}

#endif
