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

#include "ores.ore.core/store/run_store.hpp"
#include "ores.analytics.core/repository/pricing_model_config_repository.hpp"
#include "ores.analytics.core/repository/todays_market_config_repository.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/run_document_mapper.hpp"
#include "ores.ore.core/store/detail/store_helpers.hpp"
#include "ores.refdata.core/repository/curve_configuration_repository.hpp"
#include "ores.reporting.core/repository/configuration_repository.hpp"
#include "ores.reporting.core/repository/parameter_definition_repository.hpp"
#include "ores.reporting.core/repository/report_analytic_parameter_repository.hpp"
#include "ores.reporting.core/repository/report_analytic_repository.hpp"
#include "ores.reporting.core/repository/report_configuration_repository.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/repository/report_market_binding_repository.hpp"
#include "ores.reporting.core/repository/report_run_setup_repository.hpp"
#include "ores.utility/uuid/tenant_id.hpp"
#include <boost/uuid/random_generator.hpp>
#include <algorithm>
#include <filesystem>
#include <format>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <utility>

namespace ores::ore::store {

using database::context;
using detail::read_where;
using detail::stamp_party;
using reporting::domain::report_run_setup;
using namespace ores::reporting::repository;

namespace {

/**
 * @brief A configuration document the store holds: the configuration type it
 * fills, the component that owns its rows, and the run setup column that names
 * its file.
 */
struct document_kind {
    std::string_view code;
    std::string_view owning_component;
    std::optional<std::string> report_run_setup::* file;
};

// In the order an import writes them: a curve segment names its conventions,
// so the conventions are written before the curves.
constexpr document_kind document_kinds[] = {
    {"conventions", "ores.refdata", &report_run_setup::conventions_file},
    {"curve_configuration", "ores.refdata", &report_run_setup::curve_config_file},
    {"todays_market", "ores.analytics", &report_run_setup::market_config_file},
    {"pricing_engines", "ores.analytics", &report_run_setup::pricing_engines_file},
};

const document_kind* find_kind(std::string_view code) {
    for (const auto& k : document_kinds)
        if (k.code == code)
            return &k;
    return nullptr;
}

// Analytic parameter definitions are seeded once, for the system tenant.
std::vector<reporting::domain::parameter_definition>
analytic_parameter_definitions(const context& ctx) {
    const auto system = ctx.with_tenant(utility::uuid::tenant_id::system(), "ores.ore.store");
    return read_where(system, parameter_definition_repository(), [](const auto& d) {
        return d.scope == "analytic";
    });
}

template <typename Document>
void parse_file(const std::string& file, const std::string& content, Document& d) {
    try {
        domain::load_data(content, d);
    } catch (const std::exception& e) {
        throw std::invalid_argument(std::format("{}: {}", file, e.what()));
    }
}

template <typename Document, typename Mapped>
Mapped parse_and_map(const std::string& content, Mapped (*map)(const Document&)) {
    Document d;
    domain::load_data(content, d);
    return map(d);
}

// Stores one configuration document under its configuration row. A document
// with a header carries the configuration's id and name on it, which is what
// the export finds it by.
conventions_write_result store_document(const context& ctx,
                                        std::string_view code,
                                        const std::string& content,
                                        const reporting::domain::configuration& c) {
    const auto with_header = [&](auto mapped) {
        mapped.config.configuration_id = c.id;
        mapped.config.name = c.name;
        write(ctx, std::move(mapped));
    };
    if (code == "pricing_engines")
        with_header(parse_and_map(content, &domain::pricing_engine_mapper::map));
    else if (code == "todays_market")
        with_header(parse_and_map(content, &domain::todays_market_mapper::map));
    else if (code == "curve_configuration")
        with_header(parse_and_map(content, &domain::curve_configuration_mapper::map));
    else if (code == "conventions")
        return write(ctx, parse_and_map(content, &domain::conventions_mapper::map));
    return {};
}

template <typename Repository>
boost::uuids::uuid header_of(const context& ctx,
                             Repository repo,
                             const reporting::domain::report_configuration& binding) {
    for (const auto& h : repo.read_latest(ctx))
        if (h.configuration_id == binding.configuration_id)
            return h.id;
    throw std::runtime_error(std::format("No {} document holds the configuration the report "
                                         "definition binds in that slot.",
                                         binding.configuration_type_code));
}

// Conventions have no header: a binding to them means the conventions the
// session sees, which are the party's own and the tenant's world conventions.
std::string load_document(const context& ctx, const reporting::domain::report_configuration& b) {
    const auto& code = b.configuration_type_code;
    if (code == "pricing_engines")
        return domain::save_data(domain::pricing_engine_mapper::reverse(read_pricing_engines(
            ctx, header_of(ctx, analytics::repository::pricing_model_config_repository(), b))));
    if (code == "todays_market")
        return domain::save_data(domain::todays_market_mapper::reverse(read_todays_market(
            ctx, header_of(ctx, analytics::repository::todays_market_config_repository(), b))));
    if (code == "curve_configuration")
        return domain::save_data(
            domain::curve_configuration_mapper::reverse(read_curve_configuration(
                ctx, header_of(ctx, refdata::repository::curve_configuration_repository(), b))));
    return domain::save_data(domain::conventions_mapper::reverse(read_conventions(ctx)));
}

}

run_import_result import_run(const context& ctx,
                             const boost::uuids::uuid& report_definition_id,
                             const std::string& name,
                             const input_files& files) {
    const auto run_file = files.find(std::string(run_document_file));
    if (run_file == files.end())
        throw std::invalid_argument(
            std::format("An ORE input holds its run document as {}.", run_document_file));

    const auto of_definition = [&](const auto& row) {
        return row.report_definition_id == report_definition_id;
    };
    if (!read_where(ctx, report_run_setup_repository(), of_definition).empty())
        throw std::invalid_argument(
            "The report definition already holds a run document; import into a new definition.");

    domain::ore run;
    parse_file(std::string(run_document_file), run_file->second, run);

    auto next_id = boost::uuids::random_generator();
    auto setup = domain::run_document_mapper::map_setup(run);
    setup.id = next_id();
    setup.report_definition_id = report_definition_id;
    stamp_party(ctx, setup);

    auto analytics = domain::run_document_mapper::map_analytics(run);
    std::vector<reporting::domain::report_analytic> analytic_rows;
    for (auto& a : analytics) {
        a.analytic.id = next_id();
        a.analytic.report_definition_id = report_definition_id;
        stamp_party(ctx, a.analytic);
        analytic_rows.push_back(a.analytic);
    }

    auto bindings = domain::run_document_mapper::map_market_bindings(run);
    for (auto& b : bindings) {
        b.id = next_id();
        b.report_definition_id = report_definition_id;
    }
    stamp_party(ctx, bindings);

    std::map<std::pair<std::string, std::string>, boost::uuids::uuid> definition_ids;
    for (const auto& d : analytic_parameter_definitions(ctx))
        definition_ids[{d.subtype, d.name}] = d.id;

    std::vector<reporting::domain::report_analytic_parameter> parameters;
    for (const auto& a : analytics) {
        for (const auto& p : a.parameters) {
            const auto it = definition_ids.find({a.analytic.analytic_type_code, p.name});
            if (it == definition_ids.end())
                throw std::invalid_argument(
                    std::format("The {} analytic sets {}, which no parameter definition describes.",
                                a.analytic.analytic_type_code,
                                p.name));
            reporting::domain::report_analytic_parameter row;
            row.id = next_id();
            row.report_analytic_id = a.analytic.id;
            row.parameter_definition_id = it->second;
            row.value = p.value;
            row.position = p.position;
            parameters.push_back(std::move(row));
        }
    }
    stamp_party(ctx, parameters);

    // Everything an import can refuse is checked before its first write, since
    // the writes are not one transaction.
    for (const auto& kind : document_kinds) {
        const auto& file = setup.*kind.file;
        if (file && !files.contains(*file))
            throw std::invalid_argument(std::format(
                "The run document names {} as its {} file, which the input does not hold.",
                *file,
                kind.code));
    }

    report_run_setup_repository().write(ctx, setup);
    report_analytic_repository().write(ctx, analytic_rows);
    report_market_binding_repository().write(ctx, bindings);
    report_analytic_parameter_repository().write(ctx, parameters);

    run_import_result r;
    r.stored.push_back(std::string(run_document_file));
    for (const auto& kind : document_kinds) {
        const auto& file = setup.*kind.file;
        if (!file)
            continue;
        const auto& content = files.at(*file);

        reporting::domain::configuration c;
        c.id = next_id();
        c.name = name + "/" + *file;
        c.configuration_type_code = std::string(kind.code);
        c.owning_component = std::string(kind.owning_component);
        stamp_party(ctx, c);
        configuration_repository().write(ctx, c);

        conventions_write_result written;
        try {
            written = store_document(ctx, kind.code, content, c);
        } catch (const std::exception& e) {
            throw std::runtime_error(std::format("{}: {}", *file, e.what()));
        }
        if (kind.code == "conventions")
            r.conventions = std::move(written);

        reporting::domain::report_configuration binding;
        binding.id = next_id();
        binding.report_definition_id = report_definition_id;
        binding.configuration_type_code = c.configuration_type_code;
        binding.configuration_id = c.id;
        stamp_party(ctx, binding);
        report_configuration_repository().write(ctx, binding);
        r.stored.push_back(*file);
    }

    for (const auto& [file, unused] : files)
        if (std::ranges::find(r.stored, file) == r.stored.end())
            r.not_stored.push_back(file);
    return r;
}

input_files export_run(const context& session, const boost::uuids::uuid& report_definition_id) {
    const auto ctx = [&] {
        if (session.party_id())
            return session;
        const auto definition = detail::read_one(
            session, report_definition_repository(), "report definition", report_definition_id);
        return session.with_party(
            session.tenant_id(), definition.party_id, {definition.party_id}, session.actor());
    }();
    const auto of_definition = [&](const auto& row) {
        return row.report_definition_id == report_definition_id;
    };
    const auto setups = read_where(ctx, report_run_setup_repository(), of_definition);
    if (setups.empty())
        throw std::invalid_argument("The report definition holds no run document.");
    const auto& setup = setups.front();

    auto analytic_rows = read_where(ctx, report_analytic_repository(), of_definition);
    std::ranges::sort(analytic_rows, [](const auto& l, const auto& r) {
        return l.display_order < r.display_order;
    });
    std::set<boost::uuids::uuid> analytic_ids;
    for (const auto& a : analytic_rows)
        analytic_ids.insert(a.id);

    std::map<boost::uuids::uuid, std::string> definition_names;
    for (const auto& d : analytic_parameter_definitions(ctx))
        definition_names[d.id] = d.name;

    auto parameter_rows =
        read_where(ctx, report_analytic_parameter_repository(), [&](const auto& p) {
            return analytic_ids.contains(p.report_analytic_id);
        });
    std::ranges::sort(parameter_rows,
                      [](const auto& l, const auto& r) { return l.position < r.position; });

    std::vector<domain::mapped_run_analytic> analytics;
    for (const auto& a : analytic_rows) {
        domain::mapped_run_analytic m{a, {}};
        for (const auto& p : parameter_rows)
            if (p.report_analytic_id == a.id)
                m.parameters.push_back(
                    {definition_names.at(p.parameter_definition_id), p.value, p.position});
        analytics.push_back(std::move(m));
    }

    auto bindings = read_where(ctx, report_market_binding_repository(), of_definition);
    std::ranges::sort(bindings,
                      [](const auto& l, const auto& r) { return l.position < r.position; });

    domain::ore run;
    run.Setup = domain::run_document_mapper::reverse_setup(setup);
    run.Analytics = domain::run_document_mapper::reverse_analytics(analytics);
    if (!bindings.empty())
        run.Markets = domain::run_document_mapper::reverse_market_bindings(bindings);

    input_files files;
    files[std::string(run_document_file)] = domain::save_data(run);
    for (const auto& b : read_where(ctx, report_configuration_repository(), of_definition)) {
        const auto* kind = find_kind(b.configuration_type_code);
        if (kind == nullptr)
            throw std::invalid_argument(std::format(
                "The report definition binds a {} configuration, which the export cannot write.",
                b.configuration_type_code));
        const auto& file = setup.*kind->file;
        if (!file)
            throw std::invalid_argument(std::format(
                "The report definition binds a {} configuration, but its run document names no "
                "file for it.",
                b.configuration_type_code));
        files[*file] = load_document(ctx, b);
    }
    return files;
}

input_files archive_layout(const input_files& files) {
    const auto run = files.find(std::string(run_document_file));
    if (run == files.end())
        throw std::invalid_argument(
            std::format("An ORE input holds its run document as {}.", run_document_file));

    domain::ore document;
    domain::load_data(run->second, document);
    std::string input_path = "Input";
    for (const auto& p : document.Setup.Parameter)
        if (std::string(p.name) == "inputPath" && !static_cast<const std::string&>(p).empty())
            input_path = static_cast<const std::string&>(p);

    // The input path and the file names come from a stored run document, which
    // a tenant edits, and the result names where the package writes; a path
    // that is absolute or climbs out with .. would write outside the package.
    const auto inside = [](const std::filesystem::path& p) {
        const auto normal = p.lexically_normal();
        if (p.is_absolute() || p.has_root_name() || normal.empty())
            return false;
        return std::ranges::none_of(normal, [](const auto& part) { return part == ".."; });
    };
    if (!inside(input_path))
        throw std::invalid_argument(
            std::format("The run document's input path {} leaves the package.", input_path));

    input_files layout;
    for (const auto& [name, content] : files) {
        const std::filesystem::path directory =
            name == run_document_file ? std::filesystem::path("Input") : std::filesystem::path(input_path);
        const auto path = directory / name;
        if (!inside(std::filesystem::path(name)) || !inside(path))
            throw std::invalid_argument(
                std::format("The input file {} would be written outside the package.", name));
        layout[path.lexically_normal().generic_string()] = content;
    }
    return layout;
}

}
