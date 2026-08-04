/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2026 Marco Craveiro <marco.craveiro@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License as published by the Free Software
 * Foundation; either version 3 of the License, or (at your option) any later
 * version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program; if not, write to the Free Software Foundation, Inc., 51
 * Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 */
#include "ores.marketdata.service/config/parser.hpp"
#include "ores.marketdata.service/config/parser_exception.hpp"
#include "ores.service/config/standard_service_options.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include <ostream>

namespace {

const std::string more_information("Try '--help' for more information.");
const std::string product_version("ores.marketdata.service v" ORES_VERSION);
const std::string build_info(ores::utility::version::build_info());
const std::string usage_error_msg("Usage error: ");
const std::string http_base_url_arg("http-base-url");

using boost::program_options::value;
using boost::program_options::options_description;

using ores::marketdata::service::config::options;
using ores::marketdata::service::config::parser_exception;

options_description make_options_description() {
    options_description storage("Storage");
    storage.add_options()(http_base_url_arg.c_str(),
                          value<std::string>()->default_value("http://localhost:8080"),
                          "Base URL of the storage HTTP API");

    using ores::service::config::standard_service_options;
    return standard_service_options::make_options_description("ores.marketdata.service.log", storage);
}

void print_help(const options_description& od, std::ostream& info) {
    info << "Market data ingestion and query service" << std::endl
         << std::endl
         << "Usage: ores.marketdata.service [options]" << std::endl
         << std::endl
         << od << std::endl;
}

void version(std::ostream& info) {
    info << product_version << std::endl
         << "Copyright (C) 2026 Marco Craveiro." << std::endl
         << "License GPLv3: GNU GPL version 3 or later " << "<http://gnu.org/licenses/gpl.html>."
         << std::endl
         << "This is free software: you are free to change and redistribute it." << std::endl
         << "There is NO WARRANTY, to the extent permitted by law." << std::endl;

    if (!build_info.empty()) {
        info << build_info << std::endl;
        info << "IMPORTANT: build details are NOT for security purposes." << std::endl;
    }
}

std::optional<options> parse_arguments(const std::vector<std::string>& arguments,
                                       std::ostream& info) {
    using ores::service::config::standard_service_options;

    const auto od(make_options_description());
    const auto vm(standard_service_options::parse(od, arguments, "MARKETDATA_SERVICE"));

    if (standard_service_options::wants_help(vm)) {
        print_help(od, info);
        return {};
    }

    if (standard_service_options::wants_version(vm)) {
        version(info);
        return {};
    }

    const auto std_opts(standard_service_options::read_options(vm));
    options r;
    r.logging = std_opts.logging;
    r.nats = std_opts.nats;
    r.database = std_opts.database;
    r.http_base_url = vm[http_base_url_arg].as<std::string>();
    return r;
}

}

namespace ores::marketdata::service::config {

std::optional<options> parser::parse(const std::vector<std::string>& arguments,
                                     std::ostream& info,
                                     std::ostream& err) const {

    try {
        return parse_arguments(arguments, info);
    } catch (const parser_exception& e) {
        err << usage_error_msg << e.what() << std::endl << more_information << std::endl;
        BOOST_THROW_EXCEPTION(e);
    } catch (const boost::program_options::error& e) {
        err << usage_error_msg << e.what() << std::endl << more_information << std::endl;
        BOOST_THROW_EXCEPTION(parser_exception(e.what()));
    }
}

}
