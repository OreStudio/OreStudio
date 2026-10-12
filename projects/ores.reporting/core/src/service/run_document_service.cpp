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
#include "ores.reporting.core/service/run_document_service.hpp"
#include "ores.database/repository/document_operations.hpp"
#include "ores.reporting.core/repository/configuration_analytic_parameter_repository.hpp"
#include "ores.reporting.core/repository/configuration_analytic_repository.hpp"
#include "ores.reporting.core/repository/configuration_repository.hpp"
#include "ores.reporting.core/repository/configuration_type_repository.hpp"
#include "ores.reporting.core/repository/parameter_definition_repository.hpp"
#include "ores.reporting.core/repository/report_configuration_repository.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/repository/report_market_binding_repository.hpp"
#include "ores.reporting.core/repository/report_run_setup_repository.hpp"
#include "ores.reporting.core/repository/risk_report_config_repository.hpp"
#include "ores.reporting.core/service/run_document_generator.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/algorithm/string/case_conv.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <format>
#include <map>
#include <set>
#include <stdexcept>
#include <utility>

namespace ores::reporting::service {

using namespace ores::reporting::repository;
using ores::database::repository::ids_of;
using ores::database::repository::read_where;

namespace {

// The reporting lookups are seeded once, for the system tenant.
ores::database::context system_scope(const ores::database::context& ctx) {
    return ctx.with_tenant(utility::uuid::tenant_id::system(), "ores.reporting.run_document");
}

// The slot a risk report configuration fills in a definition.
constexpr const char* run_settings_type = "risk_report_config";

// The parameter definitions of the analytics, by id.
std::map<boost::uuids::uuid, std::string>
analytic_parameter_names(const ores::database::context& ctx) {
    std::map<boost::uuids::uuid, std::string> names;
    for (const auto& d : read_where(system_scope(ctx),
                                    parameter_definition_repository(),
                                    [](const auto& d) { return d.scope == "analytic"; }))
        names[d.id] = d.name;
    return names;
}

// The analytics of a configuration with their parameters, in the order the run
// document writes them.
std::vector<messaging::run_analytic> analytics_of(const ores::database::context& ctx,
                                                  const boost::uuids::uuid& configuration_id) {
    auto analytics = read_where(ctx, configuration_analytic_repository(), [&](const auto& a) {
        return a.configuration_id == configuration_id;
    });
    std::ranges::sort(
        analytics, [](const auto& l, const auto& r) { return l.display_order < r.display_order; });
    std::set<boost::uuids::uuid> analytic_ids;
    for (const auto& a : analytics)
        analytic_ids.insert(a.id);

    const auto names = analytic_parameter_names(ctx);
    auto parameters =
        read_where(ctx, configuration_analytic_parameter_repository(), [&](const auto& p) {
            return analytic_ids.contains(p.configuration_analytic_id);
        });
    std::ranges::sort(parameters,
                      [](const auto& l, const auto& r) { return l.position < r.position; });

    std::vector<messaging::run_analytic> out;
    for (const auto& a : analytics) {
        messaging::run_analytic ra{a, {}};
        for (const auto& p : parameters) {
            if (p.configuration_analytic_id != a.id)
                continue;
            const auto name = names.find(p.parameter_definition_id);
            if (name == names.end())
                throw std::runtime_error(std::format(
                    "The {} analytic holds a parameter whose definition {} is not an analytic "
                    "parameter definition.",
                    a.analytic_type_code,
                    boost::uuids::to_string(p.parameter_definition_id)));
            ra.parameters.push_back({name->second, p.value, p.position});
        }
        out.push_back(std::move(ra));
    }
    return out;
}

domain::configuration write_configuration(const ores::database::context& ctx,
                                          const std::string& configuration_type_code,
                                          const std::string& name) {
    const auto types = read_where(system_scope(ctx),
                                  configuration_type_repository(),
                                  [&](const auto& t) { return t.code == configuration_type_code; });
    if (types.empty())
        throw std::invalid_argument(
            std::format("No configuration type has the code {}.", configuration_type_code));

    domain::configuration c;
    c.id = boost::uuids::random_generator()();
    c.name = name;
    c.configuration_type_code = configuration_type_code;
    c.owning_component = types.front().owning_component;
    ores::service::messaging::stamp_document(c, ctx);
    configuration_repository().write(ctx, c);
    return c;
}

void write_binding(const ores::database::context& ctx,
                   const boost::uuids::uuid& report_definition_id,
                   const std::string& configuration_type_code,
                   const boost::uuids::uuid& configuration_id) {
    domain::report_configuration binding;
    binding.id = boost::uuids::random_generator()();
    binding.report_definition_id = report_definition_id;
    binding.configuration_type_code = configuration_type_code;
    binding.configuration_id = configuration_id;
    ores::service::messaging::stamp_document(binding, ctx);
    report_configuration_repository().write(ctx, binding);
}

// What ORE spells with a capital, the risk configuration spells in lower case.
std::string observation_model_of(const std::optional<std::string>& value) {
    if (!value)
        return "disable";
    const auto lower = boost::algorithm::to_lower_copy(*value);
    return lower == "none" || lower == "move" || lower == "defer" ? lower : "disable";
}

}

run_document_service::run_document_service(context ctx)
    : ctx_(std::move(ctx)) {}

void run_document_service::save(const boost::uuids::uuid& report_definition_id,
                                messaging::run_document v) {
    const auto of_definition = [&](const auto& row) {
        return row.report_definition_id == report_definition_id;
    };
    if (!read_where(ctx_, report_run_setup_repository(), of_definition).empty())
        throw std::invalid_argument(
            "The report definition already holds a run document; import into a new definition.");
    if (!bindings_of_type(report_definition_id, run_settings_type).empty())
        throw std::invalid_argument(
            "The report definition already binds a risk report configuration; import into a new "
            "definition.");
    if (std::ranges::none_of(v.analytics, [](const auto& a) { return a.analytic.active == "Y"; }))
        throw std::invalid_argument("The run document switches no analytic on.");

    std::map<std::pair<std::string, std::string>, boost::uuids::uuid> definition_ids;
    for (const auto& d : read_where(system_scope(ctx_),
                                    parameter_definition_repository(),
                                    [](const auto& d) { return d.scope == "analytic"; }))
        definition_ids[{d.subtype, d.name}] = d.id;
    for (const auto& a : v.analytics)
        for (const auto& p : a.parameters)
            if (!definition_ids.contains({a.analytic.analytic_type_code, p.name}))
                throw std::invalid_argument(
                    std::format("The {} analytic sets {}, which no parameter definition describes.",
                                a.analytic.analytic_type_code,
                                p.name));

    const auto definition = ores::database::repository::read_one(
        ctx_, report_definition_repository(), "report definition", report_definition_id);
    auto next_id = boost::uuids::random_generator();

    // The run settings the document describes become a configuration that the definition
    // binds. The analytics belong to the configuration and not to the definition.
    const auto configuration = write_configuration(ctx_, run_settings_type, definition.name);

    domain::risk_report_config risk;
    risk.id = next_id();
    risk.name = definition.name;
    risk.description = "The run settings imported with the run document of " + definition.name;
    risk.configuration_id = configuration.id;
    risk.base_currency = v.setup.base_currency.value_or("USD");
    risk.observation_model = observation_model_of(v.setup.observation_model);
    risk.n_threads = std::max(1, v.setup.n_threads.value_or(1));
    risk.market_data_type = "eod";
    ores::service::messaging::stamp_document(risk, ctx_);
    risk_report_config_repository().write(ctx_, risk);

    v.setup.id = next_id();
    v.setup.report_definition_id = report_definition_id;
    for (auto& b : v.market_bindings) {
        b.id = next_id();
        b.report_definition_id = report_definition_id;
    }
    ores::service::messaging::stamp_document(v, ctx_);

    std::vector<domain::configuration_analytic> analytics;
    std::vector<domain::configuration_analytic_parameter> parameters;
    for (auto& a : v.analytics) {
        a.analytic.id = next_id();
        a.analytic.configuration_id = configuration.id;
        analytics.push_back(a.analytic);
        for (const auto& p : a.parameters) {
            domain::configuration_analytic_parameter row;
            row.id = next_id();
            row.configuration_analytic_id = a.analytic.id;
            row.parameter_definition_id =
                definition_ids.at({a.analytic.analytic_type_code, p.name});
            row.value = p.value;
            row.position = p.position;
            parameters.push_back(std::move(row));
        }
    }
    ores::service::messaging::stamp_document(parameters, ctx_);

    report_run_setup_repository().write(ctx_, v.setup);
    configuration_analytic_repository().write(ctx_, analytics);
    configuration_analytic_parameter_repository().write(ctx_, parameters);
    report_market_binding_repository().write(ctx_, v.market_bindings);

    // The binding comes last: it refuses a configuration whose analytics make no run.
    write_binding(ctx_, report_definition_id, run_settings_type, configuration.id);
}

std::optional<messaging::run_document>
run_document_service::get(const boost::uuids::uuid& report_definition_id) {
    const auto of_definition = [&](const auto& row) {
        return row.report_definition_id == report_definition_id;
    };
    const auto setups = read_where(ctx_, report_run_setup_repository(), of_definition);
    if (setups.empty())
        return std::nullopt;

    messaging::run_document r;
    r.setup = setups.front();

    const auto settings = bindings_of_type(report_definition_id, run_settings_type);
    if (!settings.empty())
        r.analytics = analytics_of(ctx_, settings.front().configuration_id);

    r.market_bindings = read_where(ctx_, report_market_binding_repository(), of_definition);
    std::ranges::sort(r.market_bindings,
                      [](const auto& l, const auto& r) { return l.position < r.position; });
    return r;
}

domain::configuration run_document_service::bind(const boost::uuids::uuid& report_definition_id,
                                                 const std::string& configuration_type_code,
                                                 const std::string& name) {
    const auto c = write_configuration(ctx_, configuration_type_code, name);
    write_binding(ctx_, report_definition_id, configuration_type_code, c.id);
    return c;
}

void run_document_service::remove(const boost::uuids::uuid& report_definition_id) {
    const auto of_definition = [&](const auto& row) {
        return row.report_definition_id == report_definition_id;
    };
    const auto market_bindings =
        read_where(ctx_, report_market_binding_repository(), of_definition);
    const auto setups = read_where(ctx_, report_run_setup_repository(), of_definition);
    const auto slots = bindings(report_definition_id);

    if (!market_bindings.empty())
        report_market_binding_repository().remove(ctx_, ids_of(market_bindings));
    if (!setups.empty())
        report_run_setup_repository().remove(ctx_, ids_of(setups));

    for (const auto& b : slots) {
        report_configuration_repository().remove(ctx_, boost::uuids::to_string(b.id));

        // A configuration other definitions bind is theirs as well, and stays.
        const auto shared =
            !read_where(ctx_, report_configuration_repository(), [&](const auto& o) {
                 return o.configuration_id == b.configuration_id;
             }).empty();
        if (shared)
            continue;

        if (b.configuration_type_code == run_settings_type) {
            const auto analytics =
                read_where(ctx_, configuration_analytic_repository(), [&](const auto& a) {
                    return a.configuration_id == b.configuration_id;
                });
            std::set<boost::uuids::uuid> analytic_ids;
            for (const auto& a : analytics)
                analytic_ids.insert(a.id);
            const auto parameters =
                read_where(ctx_, configuration_analytic_parameter_repository(), [&](const auto& p) {
                    return analytic_ids.contains(p.configuration_analytic_id);
                });
            const auto risk = read_where(ctx_, risk_report_config_repository(), [&](const auto& r) {
                return r.configuration_id == b.configuration_id;
            });
            if (!parameters.empty())
                configuration_analytic_parameter_repository().remove(ctx_, ids_of(parameters));
            if (!analytics.empty())
                configuration_analytic_repository().remove(ctx_, ids_of(analytics));
            if (!risk.empty())
                risk_report_config_repository().remove(ctx_, ids_of(risk));
        }
        configuration_repository().remove(ctx_, boost::uuids::to_string(b.configuration_id));
    }
}

std::optional<messaging::run_document>
run_document_service::get_or_generate(const boost::uuids::uuid& report_definition_id) {
    if (auto stored = get(report_definition_id))
        return stored;

    const auto risk = risk_report_config_repository().find_by_definition_id(
        ctx_, boost::uuids::to_string(report_definition_id));
    if (!risk)
        return std::nullopt;

    const auto definition = ores::database::repository::read_one(
        ctx_, report_definition_repository(), "report definition", report_definition_id);

    run_document_inputs in;
    in.report_definition_id = report_definition_id;
    in.party_id = definition.party_id;
    in.risk = *risk;
    in.analytics = analytics_of(ctx_, risk->configuration_id);
    in.asof_date = in.risk.market_data_date;
    for (const auto& b : bindings(report_definition_id))
        in.bound_types.push_back(b.configuration_type_code);
    return generate_run_document(in);
}

std::vector<domain::report_configuration>
run_document_service::bindings(const boost::uuids::uuid& report_definition_id) {
    return read_where(ctx_, report_configuration_repository(), [&](const auto& row) {
        return row.report_definition_id == report_definition_id;
    });
}

std::vector<domain::report_configuration>
run_document_service::bindings_of_type(const boost::uuids::uuid& report_definition_id,
                                       const std::string& configuration_type_code) {
    return read_where(ctx_, report_configuration_repository(), [&](const auto& row) {
        return row.report_definition_id == report_definition_id &&
               row.configuration_type_code == configuration_type_code;
    });
}

}
