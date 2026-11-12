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
#include <algorithm>
#include <ostream>

namespace {

const std::string more_information("Try '--help' for more information.");
const std::string product_version("ores.marketdata.official v" ORES_VERSION);
const std::string build_info(ores::utility::version::build_info());
const std::string usage_error_msg("Usage error: ");
const std::string help_arg("help");
const std::string version_arg("version");
const std::string frankfurter_url_arg("frankfurter-url");
const std::string nyfed_url_arg("nyfed-url");
const std::string ecb_url_arg("ecb-url");
const std::string boe_url_arg("boe-url");
const std::string boc_url_arg("boc-url");
const std::string boj_url_arg("boj-url");
const std::string norges_bank_url_arg("norges-bank-url");
const std::string riksbank_url_arg("riksbank-url");
const std::string rba_url_arg("rba-url");
const std::string bcb_url_arg("bcb-url");
const std::string banxico_url_arg("banxico-url");
const std::string bcch_url_arg("bcch-url");
const std::string cnb_url_arg("cnb-url");
const std::string mas_url_arg("mas-url");
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
        nyfed_url_arg.c_str(),
        value<std::string>()->default_value("https://markets.newyorkfed.org"),
        "Address of the New York Fed markets API")(
        ecb_url_arg.c_str(),
        value<std::string>()->default_value("https://data-api.ecb.europa.eu"),
        "Address of the ECB Data Portal API")(
        boe_url_arg.c_str(),
        value<std::string>()->default_value("https://www.bankofengland.co.uk"),
        "Address of the Bank of England's site, which serves its statistical database")(
        boc_url_arg.c_str(),
        value<std::string>()->default_value("https://www.bankofcanada.ca"),
        "Address of the Bank of Canada's site, which serves its Valet API")(
        boj_url_arg.c_str(),
        value<std::string>()->default_value("https://www.stat-search.boj.or.jp"),
        "Address of the Bank of Japan's time-series statistics site, which serves its API")(
        norges_bank_url_arg.c_str(),
        value<std::string>()->default_value("https://data.norges-bank.no"),
        "Address of Norges Bank's open data site, which serves its SDMX API")(
        riksbank_url_arg.c_str(),
        value<std::string>()->default_value("https://api.riksbank.se"),
        "Address of the Riksbank's API site, which serves its SWESTR API")(
        rba_url_arg.c_str(),
        value<std::string>()->default_value("https://www.rba.gov.au"),
        "Address of the RBA's site, which serves its statistical tables")(
        bcb_url_arg.c_str(),
        value<std::string>()->default_value("https://api.bcb.gov.br"),
        "Address of the Banco Central do Brasil's API site, which serves its SGS API")(
        banxico_url_arg.c_str(),
        value<std::string>()->default_value("https://www.banxico.org.mx"),
        "Address of Banco de Mexico's site, which serves its SIE API")(
        bcch_url_arg.c_str(),
        value<std::string>()->default_value("https://si3.bcentral.cl"),
        "Address of the Banco Central de Chile's statistical site, which serves its BDE web "
        "service")(
        cnb_url_arg.c_str(),
        value<std::string>()->default_value("https://www.cnb.cz"),
        "Address of the Czech National Bank's site, which serves its yearly CZEONIA files")(
        mas_url_arg.c_str(),
        value<std::string>()->default_value("https://eservices.mas.gov.sg"),
        "Address of the Monetary Authority of Singapore's statistics site, which serves its "
        "domestic interest rates")(
        source_arg.c_str(),
        value<std::vector<std::string>>()->composing(),
        "A source to host, as <source name>:<provider code>:<adapter code>:<key>, such as "
        "ecb-frankfurter:ecb:frankfurter:ECB. No part holds a colon (repeatable)")(
        backfill_days_arg.c_str(),
        value<std::uint32_t>()->default_value(400),
        "Days a source sends when an id is first demanded")(
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
    r.nyfed_url = vm[nyfed_url_arg].as<std::string>();
    r.ecb_url = vm[ecb_url_arg].as<std::string>();
    r.boe_url = vm[boe_url_arg].as<std::string>();
    r.boc_url = vm[boc_url_arg].as<std::string>();
    r.boj_url = vm[boj_url_arg].as<std::string>();
    r.norges_bank_url = vm[norges_bank_url_arg].as<std::string>();
    r.riksbank_url = vm[riksbank_url_arg].as<std::string>();
    r.rba_url = vm[rba_url_arg].as<std::string>();
    r.bcb_url = vm[bcb_url_arg].as<std::string>();
    r.banxico_url = vm[banxico_url_arg].as<std::string>();
    r.bcch_url = vm[bcch_url_arg].as<std::string>();
    r.cnb_url = vm[cnb_url_arg].as<std::string>();
    r.mas_url = vm[mas_url_arg].as<std::string>();
    r.backfill_days = vm[backfill_days_arg].as<std::uint32_t>();
    if (r.backfill_days < 1 || r.backfill_days > 3650)
        BOOST_THROW_EXCEPTION(parser_exception("--backfill-days is from 1 to 3650."));
    r.poll_minutes = std::max<std::uint32_t>(1, vm[poll_minutes_arg].as<std::uint32_t>());
    if (vm.count(source_arg)) {
        for (const auto& text : vm[source_arg].as<std::vector<std::string>>()) {
            auto source = ores::marketdata::official::config::parse_source(text);
            if (!source)
                BOOST_THROW_EXCEPTION(parser_exception(
                    "A source is <source name>:<provider code>:<adapter code>:<key>, not '" + text +
                    "'."));
            if (std::ranges::any_of(
                    r.sources, [&](const auto& s) { return s.source_name == source->source_name; }))
                BOOST_THROW_EXCEPTION(
                    parser_exception("The source '" + source->source_name + "' is given twice."));
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
