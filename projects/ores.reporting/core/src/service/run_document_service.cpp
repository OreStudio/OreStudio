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
#include "ores.reporting.core/repository/configuration_repository.hpp"
#include "ores.reporting.core/repository/configuration_type_repository.hpp"
#include "ores.reporting.core/repository/parameter_definition_repository.hpp"
#include "ores.reporting.core/repository/report_analytic_parameter_repository.hpp"
#include "ores.reporting.core/repository/report_analytic_repository.hpp"
#include "ores.reporting.core/repository/report_configuration_repository.hpp"
#include "ores.reporting.core/repository/report_market_binding_repository.hpp"
#include "ores.reporting.core/repository/report_run_setup_repository.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
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

    auto next_id = boost::uuids::random_generator();
    v.setup.id = next_id();
    v.setup.report_definition_id = report_definition_id;
    for (auto& a : v.analytics) {
        a.analytic.id = next_id();
        a.analytic.report_definition_id = report_definition_id;
    }
    for (auto& b : v.market_bindings) {
        b.id = next_id();
        b.report_definition_id = report_definition_id;
    }
    ores::service::messaging::stamp_document(v, ctx_);

    std::map<std::pair<std::string, std::string>, boost::uuids::uuid> definition_ids;
    for (const auto& d : read_where(system_scope(ctx_),
                                    parameter_definition_repository(),
                                    [](const auto& d) { return d.scope == "analytic"; }))
        definition_ids[{d.subtype, d.name}] = d.id;

    std::vector<domain::report_analytic> analytics;
    std::vector<domain::report_analytic_parameter> parameters;
    for (const auto& a : v.analytics) {
        analytics.push_back(a.analytic);
        for (const auto& p : a.parameters) {
            const auto it = definition_ids.find({a.analytic.analytic_type_code, p.name});
            if (it == definition_ids.end())
                throw std::invalid_argument(
                    std::format("The {} analytic sets {}, which no parameter definition describes.",
                                a.analytic.analytic_type_code,
                                p.name));
            domain::report_analytic_parameter row;
            row.id = next_id();
            row.report_analytic_id = a.analytic.id;
            row.parameter_definition_id = it->second;
            row.value = p.value;
            row.position = p.position;
            parameters.push_back(std::move(row));
        }
    }
    ores::service::messaging::stamp_document(parameters, ctx_);

    report_run_setup_repository().write(ctx_, v.setup);
    report_analytic_repository().write(ctx_, analytics);
    report_market_binding_repository().write(ctx_, v.market_bindings);
    report_analytic_parameter_repository().write(ctx_, parameters);
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

    auto analytics = read_where(ctx_, report_analytic_repository(), of_definition);
    std::ranges::sort(
        analytics, [](const auto& l, const auto& r) { return l.display_order < r.display_order; });
    std::set<boost::uuids::uuid> analytic_ids;
    for (const auto& a : analytics)
        analytic_ids.insert(a.id);

    std::map<boost::uuids::uuid, std::string> definition_names;
    for (const auto& d : read_where(system_scope(ctx_),
                                    parameter_definition_repository(),
                                    [](const auto& d) { return d.scope == "analytic"; }))
        definition_names[d.id] = d.name;

    auto parameters = read_where(ctx_, report_analytic_parameter_repository(), [&](const auto& p) {
        return analytic_ids.contains(p.report_analytic_id);
    });
    std::ranges::sort(parameters,
                      [](const auto& l, const auto& r) { return l.position < r.position; });

    for (const auto& a : analytics) {
        messaging::run_analytic ra{a, {}};
        for (const auto& p : parameters) {
            if (p.report_analytic_id != a.id)
                continue;
            const auto name = definition_names.find(p.parameter_definition_id);
            if (name == definition_names.end())
                throw std::runtime_error(std::format(
                    "The {} analytic holds a parameter whose definition {} is not an analytic "
                    "parameter definition.",
                    a.analytic_type_code,
                    boost::uuids::to_string(p.parameter_definition_id)));
            ra.parameters.push_back({name->second, p.value, p.position});
        }
        r.analytics.push_back(std::move(ra));
    }

    r.market_bindings = read_where(ctx_, report_market_binding_repository(), of_definition);
    std::ranges::sort(r.market_bindings,
                      [](const auto& l, const auto& r) { return l.position < r.position; });
    return r;
}

domain::configuration run_document_service::bind(const boost::uuids::uuid& report_definition_id,
                                                 const std::string& configuration_type_code,
                                                 const std::string& name) {
    const auto types = read_where(system_scope(ctx_),
                                  configuration_type_repository(),
                                  [&](const auto& t) { return t.code == configuration_type_code; });
    if (types.empty())
        throw std::invalid_argument(
            std::format("No configuration type has the code {}.", configuration_type_code));

    auto next_id = boost::uuids::random_generator();
    domain::configuration c;
    c.id = next_id();
    c.name = name;
    c.configuration_type_code = configuration_type_code;
    c.owning_component = types.front().owning_component;
    ores::service::messaging::stamp_document(c, ctx_);
    configuration_repository().write(ctx_, c);

    domain::report_configuration binding;
    binding.id = next_id();
    binding.report_definition_id = report_definition_id;
    binding.configuration_type_code = configuration_type_code;
    binding.configuration_id = c.id;
    ores::service::messaging::stamp_document(binding, ctx_);
    report_configuration_repository().write(ctx_, binding);
    return c;
}

void run_document_service::remove(const boost::uuids::uuid& report_definition_id) {
    const auto of_definition = [&](const auto& row) {
        return row.report_definition_id == report_definition_id;
    };
    const auto analytics = read_where(ctx_, report_analytic_repository(), of_definition);
    std::set<boost::uuids::uuid> analytic_ids;
    for (const auto& a : analytics)
        analytic_ids.insert(a.id);
    const auto parameters =
        read_where(ctx_, report_analytic_parameter_repository(), [&](const auto& p) {
            return analytic_ids.contains(p.report_analytic_id);
        });
    const auto market_bindings =
        read_where(ctx_, report_market_binding_repository(), of_definition);
    const auto setups = read_where(ctx_, report_run_setup_repository(), of_definition);
    const auto slots = bindings(report_definition_id);

    if (!parameters.empty())
        report_analytic_parameter_repository().remove(ctx_, ids_of(parameters));
    if (!analytics.empty())
        report_analytic_repository().remove(ctx_, ids_of(analytics));
    if (!market_bindings.empty())
        report_market_binding_repository().remove(ctx_, ids_of(market_bindings));
    if (!setups.empty())
        report_run_setup_repository().remove(ctx_, ids_of(setups));
    for (const auto& b : slots) {
        report_configuration_repository().remove(ctx_, boost::uuids::to_string(b.id));
        configuration_repository().remove(ctx_, boost::uuids::to_string(b.configuration_id));
    }
}

std::vector<domain::report_configuration>
run_document_service::bindings(const boost::uuids::uuid& report_definition_id) {
    return read_where(ctx_, report_configuration_repository(), [&](const auto& row) {
        return row.report_definition_id == report_definition_id;
    });
}

}
