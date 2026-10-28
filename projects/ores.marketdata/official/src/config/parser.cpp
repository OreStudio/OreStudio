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
#include "ores.marketdata.official/config/parser.hpp"
#include "ores.logging/logging_configuration.hpp"
#include "ores.marketdata.official/config/parser_exception.hpp"
#include "ores.nats/config/nats_configuration.hpp"
#include "ores.utility/program_options/common_configuration.hpp"
#include "ores.utility/program_options/environment_mapper_factory.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include <ostream>

namespace {

const std::string more_information("Try '--help' for more information.");
const std::string product_version("ores.marketdata.official v" ORES_VERSION);
const std::string build_info(ores::utility::version::build_info());
const std::string usage_error_msg("Usage error: ");
const std::string help_arg("help");
const std::string version_arg("version");
const std::string frankfurter_url_arg("frankfurter-url");
const std::string source_arg("source");
const std::string backfill_days_arg("backfill-days");
const std::string poll_minutes_arg("poll-minutes");

using boost::program_options::options_description;
using boost::program_options::value;
using boost::program_options::variables_map;

using ores::marketdata::official::config::options;
using ores::marketdata::official::config::parser_exception;

options_description make_options_description() {
    using ores::logging::logging_configuration;
    using ores::nats::config::nats_configuration;
    using ores::utility::program_options::common_configuration;

    options_description r;
    r.add(common_configuration::make_options_description());
    r.add(logging_configuration::make_options_description("ores.marketdata.official.log"));
    r.add(nats_configuration::make_options_description());
    nats_configuration::register_shared_domain();

    options_description adapter_opts("Adapter Options");
    adapter_opts.add_options()(frankfurter_url_arg.c_str(),
                               value<std::string>()->default_value("https://api.frankfurter.dev"),
                               "Address of Frankfurter: the hosted service or a team's own")(
        source_arg.c_str(),
        value<std::vector<std::string>>()->composing(),
        "A source to host, as <source name>:<provider code>:<Frankfurter key>, such as "
        "ecb-frankfurter:ecb:ECB (repeatable)")(
        backfill_days_arg.c_str(),
        value<std::uint32_t>()->default_value(400),
        "Days a source sends when a currency is first demanded")(
        poll_minutes_arg.c_str(),
        value<std::uint32_t>()->default_value(60),
        "Minutes between two reads of a source once its demand is served (minimum 1)");
    r.add(adapter_opts);
    return r;
}

void print_help(const options_description& od, std::ostream& info) {
    info << "Official rates adapter" << std::endl
         << std::endl
         << "Usage: ores.marketdata.official [options]" << std::endl
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
    using ores::logging::logging_configuration;
    using ores::nats::config::nats_configuration;
    using ores::utility::program_options::environment_mapper_factory;

    const auto od(make_options_description());
    const auto name_mapper(environment_mapper_factory::make_mapper("MARKETDATA_OFFICIAL"));

    variables_map vm;
    boost::program_options::store(
        boost::program_options::command_line_parser(arguments).options(od).run(), vm);
    boost::program_options::store(boost::program_options::parse_environment(od, name_mapper), vm);

    if (vm.count(help_arg)) {
        print_help(od, info);
        return {};
    }
    if (vm.count(version_arg)) {
        version(info);
        return {};
    }

    options r;
    r.logging = logging_configuration::read_options(vm);
    r.nats = nats_configuration::read_options(vm);
    r.frankfurter_url = vm[frankfurter_url_arg].as<std::string>();
    r.backfill_days = vm[backfill_days_arg].as<std::uint32_t>();
    r.poll_minutes = std::max<std::uint32_t>(1, vm[poll_minutes_arg].as<std::uint32_t>());
    if (vm.count(source_arg)) {
        for (const auto& text : vm[source_arg].as<std::vector<std::string>>()) {
            auto source = ores::marketdata::official::config::parse_source(text);
            if (!source)
                BOOST_THROW_EXCEPTION(parser_exception(
                    "A source is <source name>:<provider code>:<Frankfurter key>, not '" + text +
                    "'."));
            r.sources.push_back(std::move(*source));
        }
    }
    return r;
}

}

namespace ores::marketdata::official::config {

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
