/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.cli/config/parser.hpp"
#include "ores.cli/config/domain_parsers/dq_parser.hpp"
#include "ores.cli/config/domain_parsers/iam_parser.hpp"
#include "ores.cli/config/domain_parsers/refdata_parser.hpp"
#include "ores.cli/config/domain_parsers/variability_parser.hpp"
#include "ores.cli/config/entity_parsers/accounts_parser.hpp"
#include "ores.cli/config/entity_parsers/change_reason_categories_parser.hpp"
#include "ores.cli/config/entity_parsers/change_reasons_parser.hpp"
#include "ores.cli/config/entity_parsers/countries_parser.hpp"
#include "ores.cli/config/entity_parsers/currencies_parser.hpp"
#include "ores.cli/config/entity_parsers/feature_flags_parser.hpp"
#include "ores.cli/config/entity_parsers/login_info_parser.hpp"
#include "ores.cli/config/entity_parsers/permissions_parser.hpp"
#include "ores.cli/config/entity_parsers/roles_parser.hpp"
#include "ores.cli/config/parser_exception.hpp"
#include "ores.logging/logging_configuration.hpp"
#include "ores.utility/program_options/common_configuration.hpp"
#include "ores.utility/version/version.hpp"
#include <boost/program_options.hpp>
#include <boost/throw_exception.hpp>
#include <format>
#include <magic_enum/magic_enum.hpp>
#include <ostream>

namespace {

    const std::string indent("   ");
    const std::string more_information("Try --help' for more information.");
    const std::string product_version("CLI for ORE Studio v" ORES_VERSION);
    const std::string build_info(ORES_BUILD_INFO);
    const std::string usage_error_msg("Usage error: ");
    const std::string no_command_msg("No command supplied. ");

    const std::string refdata_domain_name("refdata");
    const std::string refdata_domain_desc("Reference data: currencies, countries.");

    const std::string iam_domain_name("iam");
    const std::string iam_domain_desc(
        "Identity and access management: accounts, roles, permissions, login-info.");

    const std::string dq_domain_name("dq");
    const std::string dq_domain_desc("Data quality: change-reasons, change-reason-categories.");

    const std::string variability_domain_name("variability");
    const std::string variability_domain_desc("Feature flags and variability: feature-flags.");

    const std::string operation_arg("operation");

    const std::string help_arg("help");
    const std::string version_arg("version");
    const std::string command_arg("command");
    const std::string domain_arg("domain");

    using boost::program_options::value;
    using boost::program_options::variables_map;
    using boost::program_options::parsed_options;
    using boost::program_options::options_description;
    using boost::program_options::positional_options_description;

    using ores::cli::config::options;
    using ores::cli::config::parser_exception;
    namespace entity_parsers = ores::cli::config::entity_parsers;
    namespace domain_parsers = ores::cli::config::domain_parsers;

    /**
     * @brief Creates the the top-level option descriptions that are visible to the
     * end users.
     */
    options_description make_top_level_visible_options_description() {
        using ores::logging::logging_configuration;
        using ores::utility::program_options::common_configuration;

        const auto god(common_configuration::make_options_description());
        const auto lod(logging_configuration::make_options_description("ores.cli.log"));

        options_description r;
        r.add(god).add(lod);
        return r;
    }

    /**
     * @brief Creates the the top-level option descriptions that are hidden to end
     * users.
     */
    options_description make_top_level_hidden_options_description() {
        options_description r("Commands");
        r.add_options()("domain", value<std::string>(), "Domain sub-menu.")(
            "command", value<std::string>(), "Entity command to execute.")(
            "args", value<std::vector<std::string>>(), "Arguments for command");
        return r;
    }

    /**
     * @brief Creates the positional options.
     */
    positional_options_description make_positional_options() {
        positional_options_description r;
        r.add("domain", 1).add("command", 1).add("args", -1);
        return r;
    }


    /**
     * @brief Ensures the supplied domain is a valid domain.
     */
    void validate_domain_name(const std::string& domain_name) {
        const bool is_valid(domain_name == refdata_domain_name || domain_name == iam_domain_name ||
                            domain_name == dq_domain_name ||
                            domain_name == variability_domain_name);

        if (!is_valid) {
            BOOST_THROW_EXCEPTION(
                parser_exception(std::format("Invalid or unsupported domain: {}. "
                                             "Available domains: refdata, iam, dq, variability",
                                             domain_name)));
        }
    }
}

/**
 * @brief Prints the header of the help text, applicable to all cases.
 */
void print_help_header(std::ostream& s) {
    s << "ORE Studio is a User Interface for Open Source Risk Engine (ORE)." << std::endl
      << "CLI provides a command line version of the interface." << std::endl
      << "ORE Studio is created by the ORE Studio project. " << std::endl;
}

/**
 * @brief Prints the top-level help text when no command is supplied.
 *
 * @param od top-level options.
 * @param info information stream.
 */
void print_help(const options_description& od, std::ostream& info) {
    print_help_header(info);
    info << "ores.cli uses a domain-based interface: <domain> <entity> <operation> <options>. "
         << std::endl
         << "See below for a list of valid domains and their entities. " << std::endl
         << std::endl
         << "Global options: " << std::endl
         << od << std::endl
         << "Domains: " << std::endl
         << std::endl;

    auto lambda([&](const std::string& name, const std::string& desc) {
        const unsigned int domain_width(15);
        info << indent << std::setfill(' ') << std::left << std::setw(domain_width) << name << desc
             << std::endl;
    });

    lambda(refdata_domain_name, refdata_domain_desc);
    lambda(iam_domain_name, iam_domain_desc);
    lambda(dq_domain_name, dq_domain_desc);
    lambda(variability_domain_name, variability_domain_desc);

    info << std::endl
         << "For entity and operation specific options, use: <domain> <entity> <operation> --help"
         << std::endl;
}

/**
 * @brief Print the program's version details.
 *
 * @param info information stream.
 */
void version(std::ostream& info) {
    info << product_version << std::endl
         << "Copyright (C) 2025 Marco Craveiro." << std::endl
         << "License GPLv3: GNU GPL version 3 or later "
         << "<http://gnu.org/licenses/gpl.html>." << std::endl
         << "This is free software: you are free to change and redistribute it." << std::endl
         << "There is NO WARRANTY, to the extent permitted by law." << std::endl;

    if (!build_info.empty()) {
        info << build_info << std::endl;
        info << "IMPORTANT: build details are NOT for security purposes." << std::endl;
    }
}

/**
 * @brief Contains the processing logic for when the user did not supply a
 * command in the command line.
 */
std::optional<options> handle_no_command(const bool has_version,
                                         const bool has_help,
                                         const options_description& od,
                                         std::ostream& info) {
    /*
     * The only valid options are help or version, so if those are not present
     * we can safely throw.
     */
    if (!has_version && !has_help)
        BOOST_THROW_EXCEPTION(parser_exception(no_command_msg));

    /*
     * Note that we do not mind if the user has supplied both help and version -
     * help takes priority.
     */
    if (has_help)
        print_help(od, info);
    else if (has_version)
        version(info);

    return {};
}


/**
 * @brief Contains the processing logic for when the user supplies a domain in
 * the command line.
 */
std::optional<options> handle_domain_command(const std::string& domain_name,
                                             const bool has_help,
                                             const parsed_options& po,
                                             std::ostream& info,
                                             variables_map& vm) {

    if (domain_name == refdata_domain_name) {
        return domain_parsers::handle_refdata_command(has_help, po, info, vm);
    } else if (domain_name == iam_domain_name) {
        return domain_parsers::handle_iam_command(has_help, po, info, vm);
    } else if (domain_name == dq_domain_name) {
        return domain_parsers::handle_dq_command(has_help, po, info, vm);
    } else if (domain_name == variability_domain_name) {
        return domain_parsers::handle_variability_command(has_help, po, info, vm);
    }

    // Unreachable - all domains handled above
    return {};
}

/**
 * @brief Parses the arguments supplied in the command line and converts them
 * into a configuration object.
 */
std::optional<options> parse_arguments(const std::vector<std::string>& arguments,
                                       std::ostream& info) {
    /*
     * Create the top-level command line options, parse them and retrieve the
     * results of the parsing. Note that we split then into visible and hidden
     * to avoid showing the hidden options to the user in the help description.
     */
    const auto visible(make_top_level_visible_options_description());
    const auto hidden(make_top_level_hidden_options_description());
    const auto all([&]() {
        options_description r;
        r.add(visible).add(hidden);
        return r;
    }());

    const auto po = boost::program_options::command_line_parser(arguments)
                        .options(all)
                        .positional(make_positional_options())
                        .allow_unregistered()
                        .run();

    variables_map vm;
    boost::program_options::store(po, vm);
    const bool has_domain(vm.count(domain_arg) != 0);
    const bool has_version(vm.count(version_arg) != 0);
    const bool has_help(vm.count(help_arg) != 0);

    /*
     * First, handle the simpler case where no domain is supplied. Note that we
     * only supply the visible options here.
     */
    if (!has_domain)
        return handle_no_command(has_version, has_help, visible, info);

    /*
     * If the user supplied a domain, we need to retrieve it and ensure it is
     * valid.
     */
    const auto domain_name(vm[domain_arg].as<std::string>());
    validate_domain_name(domain_name);

    /*
     * Copying the same approach as git, we also consider version to be invalid
     * at the domain level. We don't bother to handle this at program options
     * level, but instead check for the presence of the (supposedly valid,
     * according to program options) version command and throw.
     */
    if (has_version)
        BOOST_THROW_EXCEPTION(parser_exception("Option is not valid for domain: 'version'."));

    /*
     * We can now process the domain. Notice that we are supplying the
     * variables map into the handler by reference. This is because we need
     * access to the global options that may have already been setup.
     */
    return handle_domain_command(domain_name, has_help, po, info, vm);
}

namespace ores::cli::config {

    std::optional<options> parser::parse(const std::vector<std::string>& arguments,
                                         std::ostream& info,
                                         std::ostream& err) const {

        try {
            return parse_arguments(arguments, info);
        } catch (const parser_exception& e) {
            err << usage_error_msg << e.what() << std::endl << more_information << std::endl;
            BOOST_THROW_EXCEPTION(e);
        } catch (const boost::program_options::unknown_option& e) {
            std::string msg = e.what();
            msg += ": '" + e.get_option_name() + "'";
            err << usage_error_msg << msg << std::endl << more_information << std::endl;
            BOOST_THROW_EXCEPTION(parser_exception(msg));
        } catch (const boost::program_options::error& e) {
            err << usage_error_msg << e.what() << std::endl << more_information << std::endl;
            BOOST_THROW_EXCEPTION(parser_exception(e.what()));
        }
    }

}
