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
#include "ores.analytics.core/service/pricing_engines_document_service.hpp"
#include "ores.analytics.core/service/todays_market_document_service.hpp"
#include "ores.database/repository/document_operations.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/curve_configuration_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/pricing_engine_mapper.hpp"
#include "ores.ore.core/domain/run_document_mapper.hpp"
#include "ores.ore.core/domain/todays_market_mapper.hpp"
#include "ores.refdata.core/service/conventions_document_service.hpp"
#include "ores.refdata.core/service/curve_configuration_document_service.hpp"
#include "ores.reporting.core/repository/report_definition_repository.hpp"
#include "ores.reporting.core/service/run_document_service.hpp"
#include <algorithm>
#include <filesystem>
#include <format>
#include <optional>
#include <stdexcept>

namespace ores::ore::store {

using database::context;
using reporting::domain::report_run_setup;

namespace {

/**
 * @brief A configuration document a run can name: the configuration type it
 * fills and the run setup column that names its file.
 */
struct document_kind {
    std::string_view code;
    std::optional<std::string> report_run_setup::*file;
};

// In the order an import writes them: a curve segment names its conventions,
// so the conventions are written before the curves.
constexpr document_kind document_kinds[] = {
    {"conventions", &report_run_setup::conventions_file},
    {"curve_configuration", &report_run_setup::curve_config_file},
    {"todays_market", &report_run_setup::market_config_file},
    {"pricing_engines", &report_run_setup::pricing_engines_file},
};

const document_kind* find_kind(std::string_view code) {
    for (const auto& k : document_kinds)
        if (k.code == code)
            return &k;
    return nullptr;
}

template <typename Document>
Document parse_file(const std::string& file, const std::string& content) {
    Document d;
    try {
        domain::load_data(content, d);
    } catch (const std::exception& e) {
        throw std::invalid_argument(std::format("{}: {}", file, e.what()));
    }
    return d;
}

/**
 * @brief The configuration documents a run names, parsed and mapped.
 */
struct parsed_documents {
    std::optional<refdata::messaging::conventions_document> conventions;
    std::optional<refdata::messaging::curve_configuration_document> curves;
    std::optional<analytics::messaging::todays_market_document> market;
    std::optional<analytics::messaging::pricing_engines_document> engines;
};

void parse_document(std::string_view code,
                    const std::string& file,
                    const std::string& content,
                    parsed_documents& d) {
    if (code == "conventions")
        d.conventions =
            domain::conventions_mapper::map(parse_file<domain::conventions>(file, content));
    else if (code == "curve_configuration")
        d.curves = domain::curve_configuration_mapper::map(
            parse_file<domain::curveconfiguration>(file, content));
    else if (code == "todays_market")
        d.market =
            domain::todays_market_mapper::map(parse_file<domain::todaysmarket>(file, content));
    else if (code == "pricing_engines")
        d.engines =
            domain::pricing_engine_mapper::map(parse_file<domain::pricingengines>(file, content));
}

// Hands one parsed document to the component that owns it. A document with a
// header carries the configuration's id and name, which is what the export
// finds it by.
void store_document(const context& ctx,
                    std::string_view code,
                    parsed_documents& d,
                    const reporting::domain::configuration& c,
                    run_import_result& r) {
    const auto with_header = [&](auto& document) {
        document.config.configuration_id = c.id;
        document.config.name = c.name;
        return document;
    };
    if (code == "pricing_engines") {
        analytics::service::pricing_engines_document_service(ctx).save(with_header(*d.engines));
    } else if (code == "todays_market") {
        analytics::service::todays_market_document_service(ctx).save(with_header(*d.market));
    } else if (code == "curve_configuration") {
        refdata::service::curve_configuration_document_service(ctx).save(with_header(*d.curves));
    } else if (code == "conventions") {
        const auto saved = refdata::service::conventions_document_service(ctx).save(*d.conventions);
        r.world_conventions_kept = saved.world_kept;
        r.fx_conventions_skipped = saved.fx_skipped;
    }
}

template <typename Service>
boost::uuids::uuid header_of(Service& service,
                             const reporting::domain::report_configuration& binding) {
    if (const auto id = service.find_by_configuration(binding.configuration_id))
        return *id;
    throw std::runtime_error(std::format("No {} document holds the configuration the report "
                                         "definition binds in that slot.",
                                         binding.configuration_type_code));
}

// Conventions have no header: a binding to them means the conventions the
// session sees, which are the party's own and the tenant's world conventions.
std::string load_document(const context& ctx, const reporting::domain::report_configuration& b) {
    const auto& code = b.configuration_type_code;
    if (code == "pricing_engines") {
        analytics::service::pricing_engines_document_service s(ctx);
        return domain::save_data(domain::pricing_engine_mapper::reverse(s.get(header_of(s, b))));
    }
    if (code == "todays_market") {
        analytics::service::todays_market_document_service s(ctx);
        return domain::save_data(domain::todays_market_mapper::reverse(s.get(header_of(s, b))));
    }
    if (code == "curve_configuration") {
        refdata::service::curve_configuration_document_service s(ctx);
        return domain::save_data(
            domain::curve_configuration_mapper::reverse(s.get(header_of(s, b))));
    }
    return domain::save_data(domain::conventions_mapper::reverse(
        refdata::service::conventions_document_service(ctx).get()));
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

    const auto run = parse_file<domain::ore>(std::string(run_document_file), run_file->second);
    reporting::messaging::run_document document;
    document.setup = domain::run_document_mapper::map_setup(run);
    document.analytics = domain::run_document_mapper::map_analytics(run);
    document.market_bindings = domain::run_document_mapper::map_market_bindings(run);

    // Everything an import can refuse, a missing file or one that does not
    // parse, is found before its first write, since the writes are not one
    // transaction.
    parsed_documents parsed;
    for (const auto& kind : document_kinds) {
        const auto& file = document.setup.*kind.file;
        if (!file)
            continue;
        const auto content = files.find(*file);
        if (content == files.end())
            throw std::invalid_argument(std::format(
                "The run document names {} as its {} file, which the input does not hold.",
                *file,
                kind.code));
        parse_document(kind.code, *file, content->second, parsed);
    }

    reporting::service::run_document_service runs(ctx);
    runs.save(report_definition_id, document);

    run_import_result r;
    r.stored.push_back(std::string(run_document_file));
    for (const auto& kind : document_kinds) {
        const auto& file = document.setup.*kind.file;
        if (!file)
            continue;
        const auto c = runs.bind(report_definition_id, std::string(kind.code), name + "/" + *file);
        try {
            store_document(ctx, kind.code, parsed, c, r);
        } catch (const std::exception& e) {
            throw std::runtime_error(std::format("{}: {}", *file, e.what()));
        }
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
        const auto definition =
            database::repository::read_one(session,
                                           reporting::repository::report_definition_repository(),
                                           "report definition",
                                           report_definition_id);
        return session.with_party(
            session.tenant_id(), definition.party_id, {definition.party_id}, session.actor());
    }();

    reporting::service::run_document_service runs(ctx);
    const auto document = runs.get(report_definition_id);
    if (!document)
        throw std::invalid_argument("The report definition holds no run document.");

    domain::ore run;
    run.Setup = domain::run_document_mapper::reverse_setup(document->setup);
    run.Analytics = domain::run_document_mapper::reverse_analytics(document->analytics);
    if (!document->market_bindings.empty())
        run.Markets =
            domain::run_document_mapper::reverse_market_bindings(document->market_bindings);

    input_files files;
    files[std::string(run_document_file)] = domain::save_data(run);
    for (const auto& b : runs.bindings(report_definition_id)) {
        const auto* kind = find_kind(b.configuration_type_code);
        if (kind == nullptr)
            throw std::invalid_argument(std::format(
                "The report definition binds a {} configuration, which the export cannot write.",
                b.configuration_type_code));
        const auto& file = document->setup.*kind->file;
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
        const std::filesystem::path directory = name == run_document_file ?
                                                    std::filesystem::path("Input") :
                                                    std::filesystem::path(input_path);
        const auto path = directory / name;
        if (!inside(std::filesystem::path(name)) || !inside(path))
            throw std::invalid_argument(
                std::format("The input file {} would be written outside the package.", name));
        layout[path.lexically_normal().generic_string()] = content;
    }
    return layout;
}

}
