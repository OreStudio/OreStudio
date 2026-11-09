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
#ifndef ORES_IAM_MESSAGING_PROVISION_STEP_ARGUMENTS_HPP
#define ORES_IAM_MESSAGING_PROVISION_STEP_ARGUMENTS_HPP

#include "ores.iam.api/workflow/provision_tenant_workflow.hpp"
#include "ores.utility/codec/json_codec.hpp"
#include "ores.utility/codec/json_document.hpp"
#include "ores.utility/codec/member_count.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/decimal/decimal.hpp"
#include <algorithm>
#include <chrono>
#include <cstdint>
#include <optional>
#include <stdexcept>
#include <string>
#include <string_view>
#include <tuple>
#include <type_traits>
#include <variant>
#include <vector>

namespace ores::iam::messaging {

/// What the step executor does with a step command's kind.
enum class provision_step_action {
    system_provision,
    publish_system_curves,
    publish_bundle,
    import_lei_hierarchy,
    provision_party,
    load_staff,
    set_staff_passwords,
    attach_photos,
    build_calendars,
    start_auto_approval,
    end_auto_approval,
    subscribe_market_data,
    generate_trades,
    schedule_reports,
    complete_provisioning,
    start_tenant_setup,
    refuse
};

/**
 * @brief Classifies a step command's kind into the action the executor takes.
 *
 * The catalogue and the executed predicate both live in the workflow
 * definition, so this function only maps them onto the executor's actions: a
 * kind this build does not execute is refused, and the two steps the
 * definitions append -- the one that hands the tenant over and the one that
 * completes it -- are recognised even though no profile may order either.
 */
[[nodiscard]] inline provision_step_action classify_step_kind(std::string_view kind) {
    using namespace ores::iam::workflow;

    if (kind == complete_provisioning_step_kind)
        return provision_step_action::complete_provisioning;
    if (kind == start_tenant_setup_step_kind)
        return provision_step_action::start_tenant_setup;
    if (!is_executed_step_kind(kind))
        return provision_step_action::refuse;
    if (kind == system_provision_step_kind)
        return provision_step_action::system_provision;
    if (kind == publish_system_curves_step_kind)
        return provision_step_action::publish_system_curves;
    if (kind == "publish_bundle")
        return provision_step_action::publish_bundle;
    if (kind == "import_lei_hierarchy")
        return provision_step_action::import_lei_hierarchy;
    if (kind == "provision_party")
        return provision_step_action::provision_party;
    if (kind == "load_staff")
        return provision_step_action::load_staff;
    if (kind == set_staff_passwords_step_kind)
        return provision_step_action::set_staff_passwords;
    if (kind == "attach_photos")
        return provision_step_action::attach_photos;
    if (kind == "start_auto_approval")
        return provision_step_action::start_auto_approval;
    if (kind == "end_auto_approval")
        return provision_step_action::end_auto_approval;
    if (kind == "build_calendars")
        return provision_step_action::build_calendars;
    if (kind == "subscribe_market_data")
        return provision_step_action::subscribe_market_data;
    if (kind == "generate_trades")
        return provision_step_action::generate_trades;
    if (kind == schedule_reports_step_kind)
        return provision_step_action::schedule_reports;
    return provision_step_action::refuse;
}

namespace detail {

using ores::utility::codec::json_document;
using ores::utility::codec::json_value;

/**
 * @brief The step's arguments as a document, or an empty object when it states
 * none.
 *
 * A step whose arguments are not a JSON object is refused here, once, so every
 * reader below can state what it needs from an object without repeating that
 * check.
 */
[[nodiscard]] inline json_document read_step_arguments(const std::string& arguments_json) {
    if (arguments_json.empty())
        return json_document::make_object();

    auto parsed = json_document::parse(arguments_json);
    if (!parsed)
        throw std::runtime_error("The step's arguments are not JSON.");
    if (!parsed->root().is_object())
        throw std::runtime_error("The step's arguments are not a JSON object.");
    return std::move(*parsed);
}

/// The member an object states under the field, or a value that does not exist.
[[nodiscard]] inline json_value member(const json_value& arguments, std::string_view field) {
    return arguments.member(field);
}

[[nodiscard]] inline std::vector<std::string> read_string_list(const json_value& value,
                                                               std::string_view field) {
    if (!value.is_array())
        throw std::runtime_error("The step's argument '" + std::string(field) +
                                 "' is not a list of strings.");

    std::vector<std::string> result;
    result.reserve(value.size());
    for (const auto& element : value.elements()) {
        if (!element.is_string())
            throw std::runtime_error("The step's argument '" + std::string(field) +
                                     "' is not a list of strings.");
        result.push_back(element.text_or());
    }
    return result;
}

[[nodiscard]] inline std::string read_string(const json_value& arguments, std::string_view field) {
    const auto value = member(arguments, field);
    if (!value.exists())
        return {};
    if (!value.is_string())
        throw std::runtime_error("The step's argument '" + std::string(field) +
                                 "' is not a string.");
    return value.text_or();
}

[[nodiscard]] inline bool read_bool(const json_value& arguments, std::string_view field) {
    const auto value = member(arguments, field);
    if (!value.exists())
        return false;
    const auto flag = value.flag();
    if (!flag)
        throw std::runtime_error("The step's argument '" + std::string(field) +
                                 "' is not a boolean.");
    return *flag;
}

[[nodiscard]] inline std::vector<json_value> read_object_list(const json_value& value,
                                                              std::string_view field) {
    if (!value.is_array())
        throw std::runtime_error("The step's argument '" + std::string(field) +
                                 "' is not a list of objects.");

    std::vector<json_value> result;
    result.reserve(value.size());
    for (const auto& element : value.elements()) {
        if (!element.is_object())
            throw std::runtime_error("The step's argument '" + std::string(field) +
                                     "' is not a list of objects.");
        result.push_back(element);
    }
    return result;
}

/// A string a kind cannot act without. The context names what carries the field,
/// so the message says which entry of a list has to be fixed.
[[nodiscard]] inline std::string read_required_string(const json_value& arguments,
                                                      std::string_view field,
                                                      std::string_view context) {
    auto value = read_string(arguments, field);
    if (value.empty())
        throw std::runtime_error("The step's " + std::string(context) + " names no '" +
                                 std::string(field) + "' argument.");
    return value;
}

/// The bundle codes a step's arguments name. A step that names no bundle is a
/// profile row that asked for no data, so the field is required rather than
/// treated as a no-op.
[[nodiscard]] inline std::vector<std::string> read_bundles(const json_value& arguments) {
    const auto found = member(arguments, "bundles");
    if (!found.exists())
        throw std::runtime_error("The step names no 'bundles' argument.");

    auto bundles = read_string_list(found, "bundles");
    if (bundles.empty())
        throw std::runtime_error("The step names no bundle in its 'bundles' argument.");
    return bundles;
}

[[nodiscard]] inline std::string
parameter_value(const std::vector<ores::iam::workflow::provision_tenant_parameter>& parameters,
                std::string_view name) {
    for (const auto& parameter : parameters)
        if (parameter.name == name)
            return parameter.value;
    return {};
}

} // namespace detail

/**
 * @brief The bundle codes a step publishes, in the order it names them.
 *
 * publish_bundle and provision_party share this shape: a 'bundles' list the
 * profile states. A step that names no bundle is refused rather than treated as
 * a no-op, because a profile row that asked for no data is a mistake.
 */
[[nodiscard]] inline std::vector<std::string>
parse_step_bundles(const std::string& arguments_json) {
    return detail::read_bundles(detail::read_step_arguments(arguments_json).root());
}

/**
 * @brief Puts a run's parameter values in place of every @c {name} a text
 * names.
 *
 * A profile's row states the data it wants, and part of that data is a value a
 * person typed: the counterparty set a starting point publishes is the value
 * of its @c counterparty_size parameter, so the row names the parameter rather
 * than one of its values. This is the same reading an LEI import does when its
 * row leaves the root LEI out, written so a code inside a longer dataset code
 * can be built from it.
 *
 * A reference the run supplies no value for is refused rather than left in
 * place: a step that published without the member its row named would leave the
 * tenant with half the data the person asked for.
 */
[[nodiscard]] inline std::string resolve_parameter_references(
    std::string text,
    const std::vector<ores::iam::workflow::provision_tenant_parameter>& parameters) {
    auto opening = text.find('{');
    while (opening != std::string::npos) {
        const auto closing = text.find('}', opening);
        if (closing == std::string::npos)
            throw std::runtime_error("The step's argument '" + text +
                                     "' names a parameter without closing its braces.");

        const auto name = text.substr(opening + 1, closing - opening - 1);
        const auto value = detail::parameter_value(parameters, name);
        if (value.empty())
            throw std::runtime_error("The step's argument '" + text + "' names the parameter '" +
                                     name + "', which the run supplies no value for.");

        text.replace(opening, closing - opening + 1, value);
        opening = text.find('{', opening + value.size());
    }
    return text;
}

/// The bundles a publish_bundle step publishes, and the optional members of
/// them its profile opts in.
struct publish_bundle_step_arguments {
    std::vector<std::string> bundles;
    std::vector<std::string> opted_in_datasets;
};

/**
 * @brief Reads a publish_bundle step's arguments.
 *
 * The bundles are the profile's data. The opted-in datasets are the members of
 * those bundles that a bundle publishes only when a profile asks for them, so a
 * profile that names none publishes the bundles whole and one that names a
 * member has it published beside the rest. A code may name a run parameter in
 * braces; see @ref resolve_parameter_references.
 */
[[nodiscard]] inline publish_bundle_step_arguments parse_publish_bundle_arguments(
    const std::string& arguments_json,
    const std::vector<ores::iam::workflow::provision_tenant_parameter>& parameters) {
    publish_bundle_step_arguments result;
    result.bundles = parse_step_bundles(arguments_json);

    const auto arguments = detail::read_step_arguments(arguments_json);
    if (const auto found = detail::member(arguments.root(), "opted_in_datasets"))
        result.opted_in_datasets = detail::read_string_list(found, "opted_in_datasets");

    for (auto& dataset : result.opted_in_datasets)
        dataset = resolve_parameter_references(dataset, parameters);
    return result;
}

/// The bundles a provision_party step publishes, and the one party it acts on.
struct provision_party_step_arguments {
    std::vector<std::string> bundles;
    /// The party to provision, named by its identifier or its exact full name.
    /// Empty means every party the tenant holds, which is what a tenant's own
    /// run asks for: the parties its hierarchy import created.
    std::string party;
    /**
     * The legal entity the party was built from, when it was built from one the
     * deployment holds.
     *
     * Empty for a party a person described by hand, which is a party the
     * register has no LEI for; a run that records none is the same run as one
     * whose party is not in the register.
     */
    std::string lei;
};

/**
 * @brief Reads a provision_party step's arguments.
 *
 * A profile names the bundles. A row that names no party provisions every party
 * the tenant holds, which is the tenant run's reading; a row that names one, or
 * a request that names one, provisions that party alone. The LEI is the
 * request's and never the profile's: the entity is chosen by the person who
 * adds the party, not by the starting point it is added from.
 */
[[nodiscard]] inline provision_party_step_arguments
parse_provision_party_arguments(const std::string& arguments_json) {
    provision_party_step_arguments result;
    result.bundles = parse_step_bundles(arguments_json);

    const auto arguments = detail::read_step_arguments(arguments_json);
    result.party = detail::read_string(arguments.root(), "party");
    result.lei = detail::read_string(arguments.root(), "lei");
    return result;
}

/// The bundles an import_lei_hierarchy step publishes and the root LEI it
/// imports under.
struct lei_hierarchy_step_arguments {
    std::vector<std::string> bundles;
    std::string root_lei;
};

/**
 * @brief Reads an import_lei_hierarchy step's arguments.
 *
 * The profile names the bundle that carries the hierarchy. The root LEI may
 * come from the step's arguments or, when the row states none, from the run's
 * parameters, which is how the form supplies it. A run that supplies neither
 * imports nothing rather than being refused: a starting point may leave the
 * legal entity out, and its tenant is then built from the party it creates for
 * itself. The step says it had nothing to import, which the run reports as a
 * warning, because a run that silently did nothing where it promised work is
 * worse than one that says so.
 */
[[nodiscard]] inline lei_hierarchy_step_arguments parse_lei_hierarchy_arguments(
    const std::string& arguments_json,
    const std::vector<ores::iam::workflow::provision_tenant_parameter>& parameters) {
    lei_hierarchy_step_arguments result;
    result.bundles = parse_step_bundles(arguments_json);

    const auto arguments = detail::read_step_arguments(arguments_json);
    result.root_lei = detail::read_string(arguments.root(), "root_lei");
    if (result.root_lei.empty())
        result.root_lei = detail::parameter_value(parameters, "root_lei");

    return result;
}

/// One party a kind acts on, named by its legal name, with the bundles the kind
/// publishes against it and whether it becomes the administrator's default.
struct staff_party_assignment {
    std::string party_name;
    std::vector<std::string> bundles;
    bool is_default = false;
};

/**
 * @brief Reads a load_staff step's arguments.
 *
 * The profile names each party by the legal name the LEI import created it
 * with, so the kind reaches the party through the party read rather than
 * through an office code of its own. Each entry carries the bundles that load
 * that party's staff and organisation.
 */
[[nodiscard]] inline std::vector<staff_party_assignment>
parse_staff_assignments(const std::string& arguments_json) {
    const auto arguments = detail::read_step_arguments(arguments_json);
    const auto found = detail::member(arguments.root(), "parties");
    if (!found.exists())
        throw std::runtime_error("The step names no 'parties' argument.");

    auto entries = detail::read_object_list(found, "parties");
    if (entries.empty())
        throw std::runtime_error("The step names no party in its 'parties' argument.");

    std::vector<staff_party_assignment> result;
    result.reserve(entries.size());
    for (const auto& entry : entries) {
        staff_party_assignment assignment;
        assignment.party_name = detail::read_required_string(entry, "name", "party entry");
        assignment.bundles = detail::read_bundles(entry);
        assignment.is_default = detail::read_bool(entry, "default");
        result.push_back(std::move(assignment));
    }
    return result;
}

/// The password a set_staff_passwords step gives every staff account.
struct set_staff_passwords_step_arguments {
    std::string password;
};

/**
 * @brief Reads a set_staff_passwords step's arguments.
 *
 * The password is the step's own data, read from the profile row rather than
 * baked into the build, so the one place the demonstration password is stated
 * is the starting point that needs it. The step's hands still hash it: the
 * server's password API is what turns it into a credential.
 */
[[nodiscard]] inline set_staff_passwords_step_arguments
parse_set_staff_passwords_arguments(const std::string& arguments_json) {
    const auto arguments = detail::read_step_arguments(arguments_json);
    set_staff_passwords_step_arguments result;
    result.password = detail::read_string(arguments.root(), "password");
    if (result.password.empty())
        throw std::runtime_error("The step names no 'password' argument.");
    return result;
}

/// One party an attach_photos step acts on, with the dataset that carries its
/// staff's photo keys.
struct photo_party_assignment {
    std::string party_name;
    std::string dataset;
};

/// The parties an attach_photos step dresses, and the template key of the
/// party logo it attaches to each of them.
struct photo_step_arguments {
    std::vector<photo_party_assignment> parties;
    std::string party_logo;
};

/**
 * @brief Reads an attach_photos step's arguments.
 *
 * A party's staff photos come from the dataset its entry names, so the kind
 * reads no office code and composes no dataset name of its own.
 */
[[nodiscard]] inline photo_step_arguments parse_photo_arguments(const std::string& arguments_json) {
    const auto arguments = detail::read_step_arguments(arguments_json);
    const auto found = detail::member(arguments.root(), "parties");
    if (!found.exists())
        throw std::runtime_error("The step names no 'parties' argument.");

    auto entries = detail::read_object_list(found, "parties");
    if (entries.empty())
        throw std::runtime_error("The step names no party in its 'parties' argument.");

    photo_step_arguments result;
    result.party_logo = detail::read_string(arguments.root(), "party_logo");
    result.parties.reserve(entries.size());
    for (const auto& entry : entries) {
        photo_party_assignment assignment;
        assignment.party_name = detail::read_required_string(entry, "name", "party entry");
        assignment.dataset = detail::read_required_string(entry, "dataset", "party entry");
        result.parties.push_back(std::move(assignment));
    }
    return result;
}

/// One party's own provider, and the source of it the party is routed to.
///
/// A provider or a source the entry leaves out is the step's own, so an entry names only what
/// differs and the step may state a default for the parties it does not name.
struct market_data_party_assignment {
    /// The party, named by its identifier or its exact full name.
    std::string party;
    std::string provider;
    std::string source;
};

/// The provider whose market data a subscribe_market_data step gives the parties it does not name,
/// the source they are routed to, and the parties that take another.
struct market_data_step_arguments {
    std::string provider;
    std::string source;
    std::vector<market_data_party_assignment> parties;
};

/**
 * @brief Reads a subscribe_market_data step's arguments.
 *
 * The provider is the code of a provider in the market data catalogue, and the
 * source is the name of one of its sources. A provider with no source is refused here and by the
 * provider catalogue when it is looked up, so the message names the field rather than the pair.
 *
 * A step that names no party gives every party the tenant holds the provider and source it states.
 * A step that names parties gives each of them its own, which is what a demonstration needs when
 * its legal entities are meant to take their market data from different vendors: the collections
 * publish different feeds, and a party reads the one its own collection names. A party the
 * arguments do not name takes the step's own, and a named party the tenant does not hold is
 * refused rather than passed over.
 *
 * The generators belong to the system, so this step only subscribes and routes.
 */
[[nodiscard]] inline market_data_step_arguments
parse_market_data_arguments(const std::string& arguments_json) {
    const auto arguments = detail::read_step_arguments(arguments_json);
    market_data_step_arguments result;
    result.provider = detail::read_string(arguments.root(), "provider");
    if (result.provider.empty())
        throw std::runtime_error("The step names no 'provider' argument.");
    result.source = detail::read_string(arguments.root(), "source");
    if (result.source.empty())
        throw std::runtime_error("The step names no 'source' argument.");

    if (const auto found = detail::member(arguments.root(), "parties")) {
        for (const auto& entry : detail::read_object_list(found, "parties")) {
            market_data_party_assignment assignment;
            assignment.party = detail::read_required_string(entry, "party", "'parties' entry");
            assignment.provider = detail::read_string(entry, "provider");
            assignment.source = detail::read_string(entry, "source");
            result.parties.push_back(std::move(assignment));
        }
    }
    return result;
}

/// A provider a party subscribes to, and the source of it the party is routed to.
struct market_data_feed {
    std::string provider;
    std::string source;

    bool operator==(const market_data_feed&) const = default;
};

/**
 * @brief The providers one party takes under a subscribe_market_data step.
 *
 * Each entry that names the party, by its identifier or its exact full name, adds one provider, so
 * a party can take its rates and USD spots from one vendor and its crosses from another. A provider
 * or a source the entry leaves out is the step's own. A party that no entry names takes the step's
 * own provider and source.
 */
[[nodiscard]] inline std::vector<market_data_feed>
feeds_of_party(const market_data_step_arguments& arguments, std::string_view id,
               std::string_view full_name) {
    std::vector<market_data_feed> out;
    for (const auto& entry : arguments.parties) {
        if (entry.party != id && entry.party != full_name)
            continue;
        market_data_feed feed{.provider = entry.provider.empty() ? arguments.provider
                                                                 : entry.provider,
                              .source = entry.source.empty() ? arguments.source : entry.source};
        if (std::ranges::find(out, feed) == out.end())
            out.push_back(std::move(feed));
    }
    if (out.empty())
        out.push_back({.provider = arguments.provider, .source = arguments.source});
    return out;
}

/**
 * @brief One trade generation config a generate_trades step records and runs.
 *
 * The entry states the recipe and not the scope: the party is named as the
 * LEI import created it, the book as the party's own reference data names it,
 * and the counterparties are the party's own tradeable links. So the entry
 * carries no identifier of a row that only exists once the run has published
 * one, and the same profile row is usable in any environment.
 *
 * The anchor is optional. A run that states none anchors on the day it runs,
 * because the population is meant to sit relative to the present: a recipe
 * that pinned a date would write a stale book every time it was used. The
 * count per book is optional too, and a run that states none takes the
 * generator's own.
 *
 * The trade types are optional. An entry that states none books every type of
 * its asset class; one that states some books only those, because a party
 * prices only some of the products of a class.
 */
struct trade_generation_assignment {
    std::string party = {};
    std::string name = {};
    std::string description = {};
    std::string asset_class_code = {};
    std::string book = {};
    std::string min_tenor_code = {};
    std::string max_tenor_code = {};
    std::optional<ores::utility::decimal::decimal> min_notional = {};
    std::optional<ores::utility::decimal::decimal> max_notional = {};
    std::optional<std::chrono::year_month_day> as_of_date = {};
    std::optional<std::uint64_t> seed = {};
    std::optional<std::uint32_t> trades_per_book = {};
    std::optional<std::uint32_t> structures_per_book = {};
    std::vector<std::string> trade_type_codes = {};
};

/**
 * @brief The members of trade_generation_assignment, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_generation_assignment>) {
    using ::ores::utility::codec::field;
    return std::tuple{
        field{"party", &trade_generation_assignment::party},
        field{"name", &trade_generation_assignment::name},
        field{"description", &trade_generation_assignment::description},
        field{"asset_class_code", &trade_generation_assignment::asset_class_code},
        field{"book", &trade_generation_assignment::book},
        field{"min_tenor_code", &trade_generation_assignment::min_tenor_code},
        field{"max_tenor_code", &trade_generation_assignment::max_tenor_code},
        field{"min_notional", &trade_generation_assignment::min_notional},
        field{"max_notional", &trade_generation_assignment::max_notional},
        field{"as_of_date", &trade_generation_assignment::as_of_date},
        field{"seed", &trade_generation_assignment::seed},
        field{"trades_per_book", &trade_generation_assignment::trades_per_book},
        field{"structures_per_book", &trade_generation_assignment::structures_per_book},
        field{"trade_type_codes", &trade_generation_assignment::trade_type_codes}};
}

static_assert(::ores::utility::codec::names_every_member<trade_generation_assignment>(),
              "trade_generation_assignment gained a member its wire_fields does not name.");

/// The configs a generate_trades step records, in the order it records them.
struct trade_generation_step_arguments {
    std::vector<trade_generation_assignment> configs = {};
};

/**
 * @brief The members of trade_generation_step_arguments, in the order it declares them.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<trade_generation_step_arguments>) {
    using ::ores::utility::codec::field;
    return std::tuple{field{"configs", &trade_generation_step_arguments::configs}};
}

static_assert(::ores::utility::codec::names_every_member<trade_generation_step_arguments>(),
              "trade_generation_step_arguments gained a member its wire_fields does not name.");

/**
 * @brief Reads a generate_trades step's arguments.
 *
 * The fields a kind cannot act without are named when the entry leaves one
 * out, so a profile row that is half-written is refused before a config is
 * recorded against it rather than after.
 */
[[nodiscard]] inline std::vector<trade_generation_assignment>
parse_trade_generation_assignments(const std::string& arguments_json) {
    /*
     * A field an entry leaves out takes the default the entry states, so the
     * checks below name the one that is missing rather than reporting a
     * positional parse failure against the whole object.
     */
    auto parsed =
        ores::utility::codec::json::read_lenient<trade_generation_step_arguments>(arguments_json);
    if (!parsed)
        throw std::runtime_error("The step's arguments are not a trade generation recipe: " +
                                 parsed.error());

    auto result = std::move(parsed->configs);
    if (result.empty())
        throw std::runtime_error("The step names no config in its 'configs' argument.");
    for (const auto& entry : result) {
        const auto require = [&entry](const std::string& value, const char* field) {
            if (value.empty())
                throw std::runtime_error("The step's config '" + entry.party + "' names no '" +
                                         field + "' argument.");
        };
        require(entry.party, "party");
        require(entry.name, "name");
        require(entry.asset_class_code, "asset_class_code");
        require(entry.book, "book");
        require(entry.min_tenor_code, "min_tenor_code");
        require(entry.max_tenor_code, "max_tenor_code");
        if (!entry.min_notional || !entry.max_notional)
            throw std::runtime_error("The step's config '" + entry.name +
                                     "' states no notional band.");
        if (!entry.seed)
            throw std::runtime_error("The step's config '" + entry.name + "' states no seed.");
        if (entry.trades_per_book && *entry.trades_per_book == 0)
            throw std::runtime_error("The step's config '" + entry.name +
                                     "' states a trades_per_book of 0.");
    }
    return result;
}

/**
 * @brief The parties whose report definitions a schedule_reports step schedules.
 *
 * The step names the parties and nothing else. What to schedule is not the
 * profile's to state: each party's published definitions are the catalogue, and
 * each one's schedule is the schedule the catalogue gives it. A profile that
 * named definitions would be a copy of the catalogue that goes stale, which is
 * the state this step exists to end.
 */
struct report_schedule_step_arguments {
    std::vector<std::string> parties;
};

/**
 * @brief Reads a schedule_reports step's arguments.
 *
 * A party is named by the legal name the LEI import created it with, as the
 * staff step names it, so the step reaches the party through the party read
 * rather than through an identifier that only exists once a run has created it.
 */
[[nodiscard]] inline report_schedule_step_arguments
parse_report_schedule_arguments(const std::string& arguments_json) {
    const auto arguments = detail::read_step_arguments(arguments_json);
    const auto found = detail::member(arguments.root(), "parties");
    if (!found.exists())
        throw std::runtime_error("The step names no 'parties' argument.");

    report_schedule_step_arguments result;
    result.parties = detail::read_string_list(found, "parties");
    if (result.parties.empty())
        throw std::runtime_error("The step names no party in its 'parties' argument.");
    for (const auto& party : result.parties)
        if (party.empty())
            throw std::runtime_error("The step names an empty party in its 'parties' argument.");
    return result;
}

}

#endif
