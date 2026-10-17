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
#include "ores.logging/make_logger.hpp"
#include "ores.ore.core/domain/conventions_mapper.hpp"
#include "ores.ore.core/domain/curve_configuration_mapper.hpp"
#include "ores.ore.core/domain/domain.hpp"
#include "ores.ore.core/domain/pricing_engine_mapper.hpp"
#include "ores.ore.core/domain/run_document_mapper.hpp"
#include "ores.ore.core/domain/todays_market_mapper.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.service/messaging/nats_call.hpp"
#include "ores.refdata.api/domain/convention_family_registry.hpp"
#include "ores.refdata.api/messaging/configuration_document_protocol.hpp"
#include "ores.refdata.api/messaging/bundle_import_operations_protocol.hpp"
#include "ores.reporting.api/domain/file_type_lookup.hpp"
#include "ores.reporting.api/messaging/file_type_protocol.hpp"
#include "ores.reporting.api/messaging/run_document_protocol.hpp"
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <format>
#include <optional>
#include <rfl/json.hpp>
#include <stdexcept>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace ores::ore::service::messaging {

namespace an = ores::analytics::messaging;
namespace rd = ores::refdata::messaging;
namespace rp = ores::reporting::messaging;
using ores::nats::service::nats_client;
using ores::ore::messaging::run_configuration_import_execute_result;
using ores::ore::messaging::saved_document;

namespace {

inline std::string_view logger_name = "ores.ore.service.messaging.run_configuration_operations";

auto& run_configuration_operations_lg() {
    using namespace ores::logging;
    static auto instance = make_logger(logger_name);
    return instance;
}

constexpr std::string_view run_document_file = "ore.xml";

/**
 * @brief A configuration document a run can name: the configuration type it
 * fills and the Setup parameter that names its file.
 *
 * The parameter is not written here. It comes from the file types table, which
 * holds the name of every ORE parameter that carries a file.
 */
struct document_kind {
    std::string_view code;
    std::string parameter;
};

std::optional<std::string> file_of(const rp::run_document& document, std::string_view parameter) {
    for (const auto& f : document.files)
        if (f.parameter == parameter)
            return f.name;
    return std::nullopt;
}

// The configuration types an import stores, in the order it stores them: a curve
// segment names its conventions, so the conventions go first.
constexpr std::string_view imported_types[] = {
    "conventions",
    "curve_configuration",
    "todays_market",
    "pricing_engines",
};

// The run settings are no input file: the run document is built from them.
constexpr std::string_view run_settings_type = "risk_report_config";

const document_kind* find_kind(const std::vector<document_kind>& kinds, std::string_view code) {
    for (const auto& k : kinds)
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

const rfl::Generic* member_of(const rfl::Generic::Object& object, std::string_view key) {
    const auto it =
        std::ranges::find_if(object, [key](const auto& entry) { return entry.first == key; });
    return it == object.end() ? nullptr : &it->second;
}

const std::vector<rfl::Generic>* array_of(const rfl::Generic& value) {
    return std::get_if<std::vector<rfl::Generic>>(&value.get());
}

const std::vector<rfl::Generic>* first_array_of(const rfl::Generic::Object& object) {
    for (const auto& [key, value] : object)
        if (const auto* array = array_of(value))
            return array;
    return nullptr;
}

std::string string_member(const rfl::Generic& value, std::string_view key) {
    const auto* object = std::get_if<rfl::Generic::Object>(&value.get());
    if (object == nullptr)
        return {};
    const auto* member = member_of(*object, key);
    if (member == nullptr)
        return {};
    const auto* text = std::get_if<std::string>(&member->get());
    return text == nullptr ? std::string{} : *text;
}

// Sends a request the registry names at runtime and returns the decoded
// response. The families differ only in their subject and their value type, so
// the wire carries the request as the generic value the codec knows.
rfl::Generic
call_subject(nats_client& owners, std::string_view subject, const rfl::Generic& request) {
    const auto& codec = ores::nats::default_wire_codec();
    const auto msg = owners.authenticated_request(subject, codec.encode(request));
    const auto error = msg.headers.find("X-Error");
    if (error != msg.headers.end())
        throw std::runtime_error(std::format("Service error on {}: {}", subject, error->second));
    auto decoded = codec.decode<rfl::Generic>(msg.data);
    if (!decoded)
        throw std::runtime_error(
            std::format("Failed to parse response from {}: {}", subject, decoded.error().what()));
    return *decoded;
}

// The list request every family's protocol reads: one page, no filter, the
// default order.
rfl::Generic family_list_request(std::uint64_t offset, std::uint64_t limit) {
    rfl::Generic::Object order;
    order.insert(std::string{"field"}, rfl::Generic{std::string{}});
    order.insert(std::string{"descending"}, rfl::Generic{false});

    rfl::Generic::Object request;
    request.insert(std::string{"offset"}, rfl::Generic{static_cast<std::int64_t>(offset)});
    request.insert(std::string{"limit"}, rfl::Generic{static_cast<std::int64_t>(limit)});
    request.insert(std::string{"order"}, rfl::Generic{std::move(order)});
    request.insert(std::string{"filter"}, rfl::Generic{rfl::Generic::Null});
    request.insert(std::string{"as_of"}, rfl::Generic{rfl::Generic::Null});
    return rfl::Generic{std::move(request)};
}

// One family's rows, paged, narrowed to the run's party when the family's
// table carries a party. World families are the tenant's, so every row stays.
std::vector<rfl::Generic> read_family(nats_client& owners,
                                      const ores::refdata::domain::convention_family_entry& family,
                                      const std::string& party_id) {
    const auto subject = std::string(family.subject_stem) + ".list";
    constexpr std::uint64_t page = 1000;
    std::vector<rfl::Generic> rows;
    for (std::uint64_t offset = 0;; offset += page) {
        const auto response = call_subject(owners, subject, family_list_request(offset, page));
        const auto* object = std::get_if<rfl::Generic::Object>(&response.get());
        if (object == nullptr)
            throw std::runtime_error(
                std::format("The response from {} is not an object.", subject));
        const auto* array = first_array_of(*object);
        if (array == nullptr)
            break;
        for (const auto& row : *array)
            if (!family.party_scoped || string_member(row, "party_id") == party_id)
                rows.push_back(row);
        if (array->size() < page)
            break;
    }
    return rows;
}

// Stages one message per row, because a stage carries one family value.
void stage_conventions(nats_client& owners,
                       const boost::uuids::uuid& import_id,
                       const rd::conventions_document& document) {
    const auto parsed = rfl::json::read<rfl::Generic>(rfl::json::write(document));
    if (!parsed)
        throw std::runtime_error(
            std::format("The conventions document does not serialise: {}", parsed.error().what()));
    const auto* object = std::get_if<rfl::Generic::Object>(&parsed->get());
    if (object == nullptr)
        throw std::runtime_error("The conventions document is not an object.");

    for (const auto& family : ores::refdata::domain::convention_families) {
        const auto* member = member_of(*object, family.key);
        if (member == nullptr)
            continue;
        const auto* rows = array_of(*member);
        if (rows == nullptr)
            continue;
        int sequence = 0;
        for (const auto& row : *rows) {
            const auto staged =
                call(owners,
                     rd::stage_bundle_family_request{.import_id = import_id,
                                                          .family = std::string(family.key),
                                                          .sequence = sequence++,
                                                          .payload = rfl::json::write(row)});
            if (!staged.success)
                throw std::runtime_error(staged.message);
        }
    }
}

// Begins an import, stages every family, then commits them in one transaction.
rd::commit_bundle_import_response save_conventions(nats_client& owners,
                                                        const rd::conventions_document& document) {
    const auto import_id = boost::uuids::random_generator()();
    const auto begun = call(owners, rd::begin_bundle_import_request{.id = import_id});
    if (!begun.success)
        throw std::runtime_error(begun.message);
    stage_conventions(owners, import_id, document);
    auto committed = call(owners, rd::commit_bundle_import_request{.import_id = import_id});
    if (!committed.success)
        throw std::runtime_error(committed.message);
    return committed;
}

// Reassembles the document from one paged read per family. No family carries
// the whole document, and no read spans the others.
rd::conventions_document read_conventions(nats_client& owners, const std::string& party_id) {
    rfl::Generic::Object document;
    for (const auto& family : ores::refdata::domain::convention_families)
        document.insert(std::string(family.key),
                        rfl::Generic{read_family(owners, family, party_id)});
    // FX conventions have no store, as they have none on the write path.
    document.insert(std::string{"fx"}, rfl::Generic{std::vector<rfl::Generic>{}});

    const auto parsed = rfl::json::read<rd::conventions_document>(
        rfl::json::write(rfl::Generic{std::move(document)}));
    if (!parsed)
        throw std::runtime_error(
            std::format("The per-family reads do not assemble a conventions document: {}",
                        parsed.error().what()));
    return *parsed;
}

// The documents a run names, each with the Setup parameter the file types table gives it.
std::vector<document_kind> document_kinds_of(nats_client& owners) {
    rp::file_types_filter filter;
    filter.analytic_type_code = std::optional<std::string>{};
    filter.configuration_type_code_one_of =
        std::vector<std::string>(std::begin(imported_types), std::end(imported_types));
    const auto reply = call(owners, rp::list_file_types_request{.limit = 100, .filter = filter});

    std::vector<document_kind> kinds;
    for (const auto code : imported_types) {
        const auto* t = ores::reporting::domain::setup_file_type_of(reply.types, code);
        if (t == nullptr)
            throw std::invalid_argument(
                std::format("No file type names the document of the {} configuration type.", code));
        kinds.push_back({code, t->ore_parameter});
    }
    return kinds;
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
    const auto with_header = [&](auto& document) -> auto& {
        document.config.configuration_id = parse_id(configuration_id);
        document.config.name = configuration_name;
        return document;
    };
    if (code == "conventions") {
        auto committed = save_conventions(owners, *d.conventions);
        r.world_conventions_kept = std::move(committed.world_kept);
        for (const auto& fx : d.conventions->fx)
            r.fx_conventions_skipped.push_back(fx.pair.base_currency + "-" +
                                               fx.pair.quote_currency + "-FX-CONVENTIONS");
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
    auto setup = ores::ore::domain::run_document_mapper::map_setup(run);
    document.settings = std::move(setup.settings);
    document.files = std::move(setup.files);
    document.analytics = ores::ore::domain::run_document_mapper::map_analytics(run);
    document.market_bindings = ores::ore::domain::run_document_mapper::map_market_bindings(run);

    // Everything an import can refuse, a missing file or one that does not
    // parse, is found before its first write.
    const auto kinds = document_kinds_of(owners);
    parsed_documents parsed;
    for (const auto& kind : kinds) {
        const auto file = file_of(document, kind.parameter);
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
        for (const auto& kind : kinds) {
            const auto file = file_of(document, kind.parameter);
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
    // Deleting the run document also deletes every binding and the
    // configuration row it names, including a binding whose document never
    // stored, so a failed store leaves no orphan.
    if (run_document_saved)
        call(owners, rp::delete_run_document_request{.report_definition_id = report_definition_id});
}

input_files export_run(nats_client& owners, const std::string& report_definition_id) {
    using namespace ores::ore::domain;
    const auto run =
        call(owners, rp::get_run_document_request{.report_definition_id = report_definition_id});

    const auto kinds = document_kinds_of(owners);
    ore document;
    document.Setup =
        run_document_mapper::reverse_setup({run.document.settings, run.document.files});
    document.Analytics = run_document_mapper::reverse_analytics(run.document.analytics);
    if (!run.document.market_bindings.empty())
        document.Markets =
            run_document_mapper::reverse_market_bindings(run.document.market_bindings);

    input_files files;
    files[std::string(run_document_file)] = save_data(document);
    for (const auto& b : run.bindings) {
        if (b.configuration_type_code == run_settings_type)
            continue;
        const auto* kind = find_kind(kinds, b.configuration_type_code);
        if (kind == nullptr)
            throw std::invalid_argument(std::format(
                "The report definition binds a {} configuration, which the export cannot write.",
                b.configuration_type_code));
        const auto file = file_of(run.document, kind->parameter);
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
            files[*file] =
                save_data(conventions_mapper::reverse(read_conventions(owners, run.party_id)));
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

ores::ore::service::messaging::reference_curve_names
reference_curve_names_of(nats_client& owners, const std::string& report_definition_id) {
    const auto run =
        call(owners, rp::get_run_document_request{.report_definition_id = report_definition_id});

    /*
     * The run's own two documents. The curve configuration says which curves the
     * party defines and what a curve's series URI is; the today's market says
     * which index the party forwards to each of them. Neither alone names the
     * curve the way ORE resolves a bond's ReferenceCurveId, so both are read.
     */
    std::unordered_map<std::string, std::string> curve_uri_of_curve_name;
    std::optional<ores::analytics::messaging::todays_market_document> market;

    for (const auto& b : run.bindings) {
        if (b.configuration_type_code == run_settings_type)
            continue;
        const auto configuration = boost::uuids::to_string(b.configuration_id);
        const auto& code = b.configuration_type_code;
        if (code == "curve_configuration") {
            const auto document =
                call(owners,
                     rd::get_curve_configuration_document_request{.configuration_id = configuration,
                                                                  .party_id = run.party_id})
                    .document;

            std::unordered_map<std::string, std::string> currency_of_definition;
            for (const auto& d : document.definitions)
                currency_of_definition[boost::uuids::to_string(d.id)] = d.curve_id;

            for (const auto& y : document.yield_curves) {
                const auto it =
                    currency_of_definition.find(boost::uuids::to_string(y.curve_definition_id));
                if (it == currency_of_definition.end() || !y.day_counter || y.day_counter->empty())
                    continue;
                /*
                 * The name the curve configuration gives the curve, which is
                 * what a today's market forwards to. ORE writes a yield curve's
                 * own name as its section, currency and curve id.
                 */
                curve_uri_of_curve_name["Yield/" + y.currency + "/" + it->second] =
                    std::format("oresmd://ir/{}?type=series&instrument=zero&quote=rate"
                                "&curve_id={}&day_counter={}",
                                y.currency,
                                it->second,
                                *y.day_counter);
            }
        } else if (code == "todays_market") {
            market = call(owners,
                          an::get_todays_market_document_request{.configuration_id = configuration,
                                                                 .party_id = run.party_id})
                         .document;
        }
    }

    std::unordered_map<std::string, std::string> index_of_curve_uri;
    if (market) {
        std::unordered_set<std::string> forwarding_collections;
        for (const auto& c : market->collections)
            if (c.collection == "IndexForwardingCurves")
                forwarding_collections.insert(boost::uuids::to_string(c.id));

        for (const auto& e : market->entries) {
            if (!e.key_value || e.key_value->empty())
                continue;
            if (!forwarding_collections.contains(
                    boost::uuids::to_string(e.todays_market_collection_id)))
                continue;
            const auto it = curve_uri_of_curve_name.find(e.target);
            if (it == curve_uri_of_curve_name.end())
                continue;
            index_of_curve_uri[it->second] = *e.key_value;
        }
    }

    BOOST_LOG_SEV(run_configuration_operations_lg(), ores::logging::debug)
        << "reference_curve_names_of: " << index_of_curve_uri.size()
        << " curve name(s) for definition " << report_definition_id;

    return [index_of_curve_uri](const std::string& uri) -> std::string {
        const auto it = index_of_curve_uri.find(uri);
        return it == index_of_curve_uri.end() ? uri : it->second;
    };
}

}
