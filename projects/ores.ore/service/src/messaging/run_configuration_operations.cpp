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
#include "ores.ore.service/messaging/run_configuration_operations.hpp"
#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/curve_configuration_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/pricing_engine_mapper.hpp"
#include "ores.ore.core/domain/run_document_mapper.hpp"
#include "ores.ore.core/domain/todays_market_mapper.hpp"
#include "ores.ore.service/messaging/nats_call.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.reporting.api/messaging/run_document_protocol.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <format>
#include <optional>
#include <stdexcept>
#include <string_view>

namespace ores::ore::service::messaging {

namespace an = ores::analytics::messaging;
namespace rd = ores::refdata::messaging;
namespace rp = ores::reporting::messaging;
using ores::nats::service::nats_client;
using ores::ore::messaging::run_configuration_import_execute_result;
using ores::ore::messaging::saved_document;
using ores::reporting::domain::report_run_setup;

namespace {

constexpr std::string_view run_document_file = "ore.xml";

/**
 * @brief A configuration document a run can name: the configuration type it
 * fills and the run setup column that names its file.
 */
struct document_kind {
    std::string_view code;
    std::optional<std::string> report_run_setup::* file;
};

// In the order an import stores them: a curve segment names its conventions,
// so the conventions go first.
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

// Sends a request and returns its response, or throws with the owner's reason.
template <typename Req>
typename Req::response_type call(nats_client& owners, const Req& req) {
    std::string error;
    auto resp = nats_call(owners, req, error);
    if (!resp || !error.empty())
        throw std::runtime_error(error.empty() ? "no response" : error);
    return *resp;
}

template <typename Document>
Document parse_file(const std::string& file, const std::string& content) {
    Document d;
    try {
        ores::ore::domain::load_data(content, d);
    } catch (const std::exception& e) {
        throw std::invalid_argument(std::format("{}: {}", file, e.what()));
    }
    return d;
}

boost::uuids::uuid parse_id(const std::string& text) {
    return boost::uuids::string_generator()(text);
}

/**
 * @brief The configuration documents a run names, parsed and mapped.
 */
struct parsed_documents {
    std::optional<rd::conventions_document> conventions;
    std::optional<rd::curve_configuration_document> curves;
    std::optional<an::todays_market_document> market;
    std::optional<an::pricing_engines_document> engines;
};

void parse_document(std::string_view code,
                    const std::string& file,
                    const std::string& content,
                    parsed_documents& d) {
    using namespace ores::ore::domain;
    if (code == "conventions")
        d.conventions = conventions_mapper::map(parse_file<conventions>(file, content));
    else if (code == "curve_configuration")
        d.curves = curve_configuration_mapper::map(parse_file<curveconfiguration>(file, content));
    else if (code == "todays_market")
        d.market = todays_market_mapper::map(parse_file<todaysmarket>(file, content));
    else if (code == "pricing_engines")
        d.engines = pricing_engine_mapper::map(parse_file<pricingengines>(file, content));
}

// Hands one parsed document to the component that owns it. A document with a
// header carries the configuration's id and name, which is what the export
// finds it by.
void store_document(nats_client& owners,
                    std::string_view code,
                    parsed_documents& d,
                    const std::string& configuration_id,
                    const std::string& configuration_name,
                    run_configuration_import_execute_result& r) {
    const auto with_header = [&](auto& document) {
        document.config.configuration_id = parse_id(configuration_id);
        document.config.name = configuration_name;
        return document;
    };
    if (code == "conventions") {
        const auto saved =
            call(owners, rd::save_conventions_document_request{.document = *d.conventions});
        r.world_conventions_kept = saved.world_kept;
        r.fx_conventions_skipped = saved.fx_skipped;
        return;
    }
    if (code == "curve_configuration")
        call(owners,
             rd::save_curve_configuration_document_request{.document = with_header(*d.curves)});
    else if (code == "todays_market")
        call(owners, an::save_todays_market_document_request{.document = with_header(*d.market)});
    else if (code == "pricing_engines")
        call(owners,
             an::save_pricing_engines_document_request{.document = with_header(*d.engines)});
    r.saved_documents.push_back(saved_document{.configuration_type_code = std::string(code),
                                               .configuration_id = configuration_id});
}

}

run_configuration_import_execute_result import_run(nats_client& owners,
                                                   const std::string& report_definition_id,
                                                   const std::string& name,
                                                   const input_files& files) {
    const auto run_file = files.find(std::string(run_document_file));
    if (run_file == files.end())
        throw std::invalid_argument(
            std::format("An ORE input holds its run document as {}.", run_document_file));

    const auto run =
        parse_file<ores::ore::domain::ore>(std::string(run_document_file), run_file->second);
    rp::run_document document;
    document.setup = ores::ore::domain::run_document_mapper::map_setup(run);
    document.analytics = ores::ore::domain::run_document_mapper::map_analytics(run);
    document.market_bindings = ores::ore::domain::run_document_mapper::map_market_bindings(run);

    // Everything an import can refuse, a missing file or one that does not
    // parse, is found before its first write.
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

    run_configuration_import_execute_result r;
    r.report_definition_id = report_definition_id;
    try {
        call(owners,
             rp::save_run_document_request{.report_definition_id = report_definition_id,
                                           .document = document});
        r.run_document_saved = true;
        r.stored.push_back(std::string(run_document_file));
        for (const auto& kind : document_kinds) {
            const auto& file = document.setup.*kind.file;
            if (!file)
                continue;
            const auto configuration_name = name + "/" + *file;
            try {
                const auto bound = call(owners,
                                        rp::bind_configuration_request{
                                            .report_definition_id = report_definition_id,
                                            .configuration_type_code = std::string(kind.code),
                                            .name = configuration_name});
                store_document(
                    owners, kind.code, parsed, bound.configuration_id, configuration_name, r);
            } catch (const std::exception& e) {
                throw std::runtime_error(std::format("{}: {}", *file, e.what()));
            }
            r.stored.push_back(*file);
        }
    } catch (const std::exception& e) {
        try {
            undo_import(owners, report_definition_id, r.run_document_saved, r.saved_documents);
        } catch (const std::exception& u) {
            throw std::runtime_error(std::format(
                "{}; deleting what the import stored also failed: {}", e.what(), u.what()));
        }
        throw;
    }

    for (const auto& [file, unused] : files)
        if (std::ranges::find(r.stored, file) == r.stored.end())
            r.not_stored.push_back(file);
    r.success = true;
    r.message = std::format("Stored {} files.", r.stored.size());
    return r;
}

void undo_import(nats_client& owners,
                 const std::string& report_definition_id,
                 bool run_document_saved,
                 const std::vector<saved_document>& saved) {
    for (auto it = saved.rbegin(); it != saved.rend(); ++it) {
        const auto& code = it->configuration_type_code;
        const auto& id = it->configuration_id;
        if (code == "curve_configuration")
            call(owners, rd::delete_curve_configuration_document_request{.configuration_id = id});
        else if (code == "todays_market")
            call(owners, an::delete_todays_market_document_request{.configuration_id = id});
        else if (code == "pricing_engines")
            call(owners, an::delete_pricing_engines_document_request{.configuration_id = id});
    }
    if (run_document_saved)
        call(owners, rp::delete_run_document_request{.report_definition_id = report_definition_id});
}

input_files export_run(nats_client& owners, const std::string& report_definition_id) {
    using namespace ores::ore::domain;
    const auto run =
        call(owners, rp::get_run_document_request{.report_definition_id = report_definition_id});

    ore document;
    document.Setup = run_document_mapper::reverse_setup(run.document.setup);
    document.Analytics = run_document_mapper::reverse_analytics(run.document.analytics);
    if (!run.document.market_bindings.empty())
        document.Markets =
            run_document_mapper::reverse_market_bindings(run.document.market_bindings);

    input_files files;
    files[std::string(run_document_file)] = save_data(document);
    for (const auto& b : run.bindings) {
        const auto* kind = find_kind(b.configuration_type_code);
        if (kind == nullptr)
            throw std::invalid_argument(std::format(
                "The report definition binds a {} configuration, which the export cannot write.",
                b.configuration_type_code));
        const auto& file = run.document.setup.*kind->file;
        if (!file)
            throw std::invalid_argument(std::format(
                "The report definition binds a {} configuration, but its run document names no "
                "file for it.",
                b.configuration_type_code));
        // The run's party owns its documents; a session that acts for no party,
        // as a workflow step's does, reads them as that party.
        const auto configuration = boost::uuids::to_string(b.configuration_id);
        const auto& code = b.configuration_type_code;
        if (code == "conventions")
            files[*file] = save_data(conventions_mapper::reverse(
                call(owners, rd::get_conventions_document_request{.party_id = run.party_id})
                    .document));
        else if (code == "curve_configuration")
            files[*file] = save_data(curve_configuration_mapper::reverse(
                call(owners,
                     rd::get_curve_configuration_document_request{.configuration_id = configuration,
                                                                  .party_id = run.party_id})
                    .document));
        else if (code == "todays_market")
            files[*file] = save_data(todays_market_mapper::reverse(
                call(owners,
                     an::get_todays_market_document_request{.configuration_id = configuration,
                                                            .party_id = run.party_id})
                    .document));
        else if (code == "pricing_engines")
            files[*file] = save_data(pricing_engine_mapper::reverse(
                call(owners,
                     an::get_pricing_engines_document_request{.configuration_id = configuration,
                                                              .party_id = run.party_id})
                    .document));
    }
    return files;
}

}
