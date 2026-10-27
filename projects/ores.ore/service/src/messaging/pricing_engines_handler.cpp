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
#include "ores.ore.service/messaging/pricing_engines_handler.hpp"
#include "ores.analytics.api/messaging/configuration_document_protocol.hpp"
#include "ores.analytics.api/messaging/pricing_engine_type_protocol.hpp"
#include "ores.nats/domain/correlation.hpp"
#include "ores.ore.api/messaging/pricing_engines_protocol.hpp"
#include "ores.ore.core/domain/pricing_engines_import.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.service/messaging/nats_call.hpp"
#include "ores.reporting.api/domain/file_type_lookup.hpp"
#include "ores.reporting.api/messaging/configuration_protocol.hpp"
#include "ores.reporting.api/messaging/file_type_protocol.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"
#include "shared_helpers.hpp"
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <algorithm>
#include <cstdint>
#include <format>
#include <map>
#include <optional>
#include <set>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

namespace ores::ore::service::messaging {

using namespace ores::logging;
using namespace ores::service::messaging;
using namespace ores::ore::messaging;

namespace {

namespace an = ores::analytics::messaging;
namespace rp = ores::reporting::messaging;

/// The configuration type whose document the export writes.
constexpr std::string_view configuration_type_code = "pricing_engines";

/**
 * @brief The name the pricing engines document is written under, or nothing
 * when no file type names it.
 *
 * The file types table names every ORE document, so the name is read rather
 * than written here: a tenant that renames the file overrides the row and the
 * export follows it. A table with no such row is a refusal, not a name
 * invented by the code (RS-ORE-01).
 */
std::optional<std::string> document_name_of(ores::nats::service::nats_client& owners) {
    // The reporting service applies the filter, so the page holds this kind's
    // rows and its limit cannot push the document's row out of the page.
    rp::file_types_filter filter;
    filter.configuration_type_code = std::string(configuration_type_code);
    std::string error;
    const auto reply =
        nats_call(owners, rp::list_file_types_request{.limit = page, .filter = filter}, error);
    if (!reply)
        throw std::runtime_error(error.empty() ? "the file types could not be read" : error);
    const auto* type =
        ores::reporting::domain::input_document_file_type_of(reply->types, configuration_type_code);
    if (type == nullptr)
        return std::nullopt;
    return type->name_pattern;
}

/**
 * @brief The product types the screens can offer, read from analytics' own
 * vocabulary.
 *
 * A type the vocabulary does not hold is not this service's to refuse: the
 * tables hold any type code, so the row is written and the person is told the
 * screens cannot offer it back.
 */
std::set<std::string> known_product_types(ores::nats::service::nats_client& owners) {
    std::set<std::string> types;
    for (std::uint32_t offset = 0;; offset += page) {
        std::string error;
        const auto reply = nats_call(
            owners, an::list_pricing_engine_types_request{.offset = offset, .limit = page}, error);
        if (!reply)
            throw std::runtime_error(error.empty() ? "the pricing engine types could not be read" :
                                                     error);
        for (const auto& type : reply->types)
            types.insert(type.code);
        if (reply->types.size() < page)
            return types;
    }
}

/**
 * @brief The document a reporting configuration holds, or an empty one when the
 * import writes into a configuration that does not exist yet.
 *
 * A target the caller names and analytics refuses is a refusal of the whole
 * operation, in analytics' own words: the file has not been compared against
 * anything, so nothing of it can be said.
 */
ores::analytics::messaging::pricing_engines_document
read_target(ores::nats::service::nats_client& owners,
            const std::string& configuration_id,
            std::string& error) {
    if (configuration_id.empty())
        return {};
    const auto read = nats_call(
        owners, an::get_pricing_engines_document_request{.configuration_id = configuration_id}, error);
    if (!read || !read->success) {
        if (error.empty())
            error = read->message.empty() ?
                        "The pricing engine configuration could not be read." :
                        read->message;
        return {};
    }
    return read->document;
}

/**
 * @brief The key each product is named by in the review, so a refusal of one
 * row is a refusal of a row the person can find.
 */
std::map<boost::uuids::uuid, std::string>
keys_of(const ores::analytics::messaging::pricing_engines_document& document) {
    std::map<boost::uuids::uuid, std::string> keys;
    std::vector<const ores::analytics::domain::pricing_model_product*> products;
    for (const auto& product : document.products)
        products.push_back(&product);
    std::ranges::sort(products, [](const auto* lhs, const auto* rhs) {
        if (lhs->position != rhs->position)
            return lhs->position < rhs->position;
        return lhs->id < rhs->id;
    });
    std::map<std::string, int> counts;
    for (const auto* product : products) {
        const auto& type_code = product->pricing_engine_type_code;
        const auto occurrence = counts[type_code]++;
        keys[product->id] = std::format("{}#{}", type_code, occurrence);
    }
    return keys;
}

/** A reporting configuration, registered for a document the import creates. */
std::string create_header(ores::nats::service::nats_client& owners,
                          const std::string& name,
                          const ores::utility::domain::change_intent& intent) {
    static thread_local ores::utility::uuid::uuid_v7_generator generator;
    const auto id = generator();
    rp::configuration_change change;
    change.write.id = id;
    change.write.name = name;
    change.write.configuration_type_code = std::string(configuration_type_code);
    change.write.owning_component = "ores.analytics";
    change.precondition = {.kind = ores::utility::domain::precondition_kind::must_not_exist};
    std::string error;
    const auto reply = nats_call(owners,
                                 rp::put_configuration_request{.change = change, .intent = intent},
                                 error);
    if (!reply)
        throw std::runtime_error(error.empty() ? "the configuration could not be registered" :
                                                 error);
    if (reply->result.outcome != ores::utility::domain::outcome::ok)
        throw std::runtime_error(reply->result.message.empty() ?
                                     "the configuration could not be registered" :
                                     reply->result.message);
    return boost::uuids::to_string(id);
}

/** Undoes the header a failed import created, so a refusal leaves nothing. */
void remove_header(ores::nats::service::nats_client& owners,
                   const std::string& name,
                   const ores::utility::domain::change_intent& intent) {
    std::string error;
    nats_call(owners,
              rp::delete_configuration_request{.removal = {.key = {.name = name}}, .intent = intent},
              error);
}

/// The identifier a request states, or a refusal naming it. The unit is built
/// with other units of the service, so the name carries this file's own word.
boost::uuids::uuid configuration_id_of(const std::string& text) {
    try {
        return boost::uuids::string_generator()(text);
    } catch (const std::exception&) {
        throw std::invalid_argument(std::format("'{}' is not an identifier.", text));
    }
}

} // namespace

pricing_engines_handler::pricing_engines_handler(ores::nats::service::client& nats,
                                                 ores::database::context ctx,
                                                 ores::security::jwt::jwt_authenticator signer,
                                                 ores::nats::service::nats_client outbound_nats)
    : nats_(nats)
    , ctx_(std::move(ctx))
    , signer_(std::move(signer))
    , outbound_nats_(std::move(outbound_nats)) {}

void pricing_engines_handler::export_pricing_engines(ores::nats::message msg) {
    const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);
    auto ctx = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx) {
        error_reply(nats_, msg, ctx.error());
        return;
    }
    // The permission the pricing engine configuration's own screen is read
    // with: the export reads the configuration, so it asks for what reading it
    // asks for. Analytics' document read states no permission of its own.
    if (!has_permission(*ctx, "analytics::pricing_model_configs:read")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    const auto req = decode<export_pricing_engines_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }

    export_pricing_engines_response response;
    try {
        auto owners = outbound_nats_.with_delegation(ores::nats::service::extract_bearer(msg))
                          .with_correlation_id(correlation_id);

        // The read is the one step whose refusal is the caller's to act on: a
        // configuration that does not exist, or that the session's party cannot
        // see, is refused by analytics in its own words.
        std::string error;
        const auto read = nats_call(
            owners,
            an::get_pricing_engines_document_request{.configuration_id = req->configuration_id},
            error);
        if (!read || !read->success) {
            response.problems.push_back(
                {.code = "document_unreadable",
                 .message = !error.empty() ? error :
                                             "The pricing engine configuration could not be "
                                             "read."});
            response.message = response.problems.front().message;
            reply(nats_, msg, response);
            return;
        }

        const auto name = document_name_of(owners);
        if (!name) {
            response.problems.push_back(
                {.code = "file_name_missing",
                 .message = std::format("No file type names the {} document. The reporting file "
                                        "types table states the name of every ORE document.",
                                        configuration_type_code)});
            response.message = response.problems.front().message;
            reply(nats_, msg, response);
            return;
        }
        response.count = static_cast<std::uint32_t>(read->document.products.size());
        response.files.push_back(
            {.name = *name,
             .content = ores::ore::xml::exporter::export_pricing_engines(read->document)});
        response.success = true;
        response.message = std::format("Exported {} products.", response.count);
        BOOST_LOG_SEV(lg(), info) << "pricing engines exported | corr=" << correlation_id
                                  << " products=" << response.count << " file=" << *name;
        reply(nats_, msg, response);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "pricing engines export refused | corr=" << correlation_id
                                  << " " << e.what();
        response.problems.push_back({.code = "write_failed", .message = e.what()});
        response.message = e.what();
        reply(nats_, msg, response);
    }
}

void pricing_engines_handler::preview_pricing_engines_import(ores::nats::message msg) {
    const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);
    auto ctx = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx) {
        error_reply(nats_, msg, ctx.error());
        return;
    }
    // The preview is the first step of a write, so it asks for what the write
    // asks for: a reader has nothing to review (RS-ORE-02).
    if (!has_permission(*ctx, "analytics::pricing_model_configs:write")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    const auto req = decode<preview_pricing_engines_import_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }

    const auto file = ores::ore::domain::pricing_engines_import::parse(req->content);
    preview_pricing_engines_import_response response;
    response.root_element = file.root_element;
    if (!file.ok()) {
        BOOST_LOG_SEV(lg(), info) << "pricing engines preview refused | corr=" << correlation_id
                                  << " " << file.refusal;
        response.message = file.refusal;
        reply(nats_, msg, response);
        return;
    }

    try {
        auto owners = outbound_nats_.with_delegation(ores::nats::service::extract_bearer(msg))
                          .with_correlation_id(correlation_id);
        std::string error;
        const auto target = read_target(owners, req->target_configuration_id, error);
        if (!error.empty()) {
            response.message = error;
            reply(nats_, msg, response);
            return;
        }

        auto difference = ores::ore::domain::pricing_engines_import::difference(
            file, target, known_product_types(owners));

        response.in_file_products = static_cast<std::uint32_t>(file.products.size());
        response.in_file_parameters = static_cast<std::uint32_t>(file.document.parameters.size());
        for (const auto& product : file.products)
            response.products.push_back({.type_code = product.type_code,
                                         .model = product.model,
                                         .engine = product.engine,
                                         .parameters = product.parameters});
        response.global_parameters = file.global_parameters;
        response.target_name = target.config.name;
        response.held = difference.held;
        response.added = difference.added;
        response.changed = difference.changed;
        response.removed = difference.removed;
        for (const auto& row : difference.rows)
            response.rows.push_back({.key = row.key,
                                     .entity = row.entity,
                                     .change = row.change,
                                     .owner = row.owner,
                                     .name = row.name,
                                     .what = row.what});
        for (const auto& issue : file.issues)
            response.issues.push_back(
                {.kind = issue.kind, .subject = issue.subject, .detail = issue.detail});
        for (const auto& issue : difference.issues)
            response.issues.push_back(
                {.kind = issue.kind, .subject = issue.subject, .detail = issue.detail});

        response.success = true;
        response.message = std::format("The file holds {} products and {} settings.",
                                       response.in_file_products,
                                       response.in_file_parameters);
        BOOST_LOG_SEV(lg(), info) << "pricing engines previewed | corr=" << correlation_id
                                  << " products=" << response.in_file_products
                                  << " added=" << response.added
                                  << " changed=" << response.changed
                                  << " removed=" << response.removed;
        reply(nats_, msg, response);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "pricing engines preview refused | corr=" << correlation_id
                                  << " " << e.what();
        response.message = e.what();
        reply(nats_, msg, response);
    }
}

void pricing_engines_handler::import_pricing_engines(ores::nats::message msg) {
    const auto correlation_id = ores::nats::extract_or_generate_correlation_id(msg);
    auto ctx = ores::service::service::make_request_context(
        ctx_, msg, std::optional<ores::security::jwt::jwt_authenticator>(signer_));
    if (!ctx) {
        error_reply(nats_, msg, ctx.error());
        return;
    }
    if (!has_permission(*ctx, "analytics::pricing_model_configs:write")) {
        error_reply(nats_, msg, ores::service::error_code::forbidden);
        return;
    }
    const auto req = decode<import_pricing_engines_request>(msg);
    if (!req) {
        error_reply(nats_, msg, ores::service::error_code::bad_request);
        return;
    }

    import_pricing_engines_response response;
    const auto file = ores::ore::domain::pricing_engines_import::parse(req->content);
    if (!file.ok()) {
        BOOST_LOG_SEV(lg(), info) << "pricing engines import refused | corr=" << correlation_id
                                  << " " << file.refusal;
        response.message = file.refusal;
        reply(nats_, msg, response);
        return;
    }

    const ores::utility::domain::change_intent intent{
        .reason_code = req->reason_code.empty() ? "system.import" : req->reason_code,
        .commentary = req->commentary.empty() ? "Imported from an ORE pricing engines document" :
                                                req->commentary};
    std::string created_header;
    try {
        auto owners = outbound_nats_.with_delegation(ores::nats::service::extract_bearer(msg))
                          .with_correlation_id(correlation_id);

        // The target: an existing configuration, or one registered here first.
        // The screen's own create writes the reporting header and then the
        // document, and both clients of this operation need that order, so the
        // order lives here rather than in each of them.
        std::string read_error;
        auto target = read_target(owners, req->target_configuration_id, read_error);
        if (!read_error.empty()) {
            response.message = read_error;
            reply(nats_, msg, response);
            return;
        }
        std::string configuration_id;
        std::string name;
        std::string variant;
        if (!req->target_configuration_id.empty()) {
            configuration_id = req->target_configuration_id;
            name = target.config.name;
            variant = target.config.config_variant;
        } else {
            name = req->new_configuration_name;
            variant = req->new_configuration_variant;
            if (name.empty()) {
                response.message = "A new configuration needs a name.";
                reply(nats_, msg, response);
                return;
            }
            configuration_id = create_header(owners, name, intent);
            created_header = name;
        }

        const auto difference = ores::ore::domain::pricing_engines_import::difference(
            file, target, known_product_types(owners));
        auto document = ores::ore::domain::pricing_engines_import::merge(
            file, target, req->removals_kept);

        const auto configuration = configuration_id_of(configuration_id);
        if (target.config.name.empty())
            document.config.id = file.document.config.id;
        document.config.name = name;
        document.config.config_variant = variant;
        document.config.configuration_id = configuration;
        // The document save carries no reason of its own, so the reason travels
        // on the rows, which the save keeps when a row names one.
        for (auto& row : document.products) {
            row.change_reason_code = intent.reason_code;
            row.change_commentary = intent.commentary;
        }
        for (auto& row : document.parameters) {
            row.change_reason_code = intent.reason_code;
            row.change_commentary = intent.commentary;
        }
        document.config.change_reason_code = intent.reason_code;
        document.config.change_commentary = intent.commentary;

        const auto keys = keys_of(document);
        std::string error;
        const auto saved = nats_call(
            owners,
            an::save_pricing_engines_document_request{.document = std::move(document)},
            error);
        if (!saved || !error.empty()) {
            if (!created_header.empty())
                remove_header(owners, created_header, intent);
            response.message = error.empty() ? "The configuration could not be saved." : error;
            reply(nats_, msg, response);
            return;
        }
        if (!saved->success) {
            if (!created_header.empty())
                remove_header(owners, created_header, intent);
            response.message = saved->message.empty() ? "The configuration was refused." :
                                                        saved->message;
            for (const auto& problem : saved->problems) {
                std::string key;
                if (!problem.product_id.empty()) {
                    for (const auto& [id, product_key] : keys)
                        if (boost::uuids::to_string(id) == problem.product_id)
                            key = problem.parameter_name.empty() ?
                                      product_key :
                                      std::format("{}/{}/{}",
                                                  product_key,
                                                  problem.parameter_scope,
                                                  problem.parameter_name);
                }
                response.errors.push_back({.key = key, .message = problem.message});
            }
            BOOST_LOG_SEV(lg(), warn) << "pricing engines import refused | corr="
                                      << correlation_id << " " << response.message;
            reply(nats_, msg, response);
            return;
        }

        response.success = true;
        response.configuration_id = saved->id;
        response.configuration_name = name;
        response.added = difference.added;
        response.changed = difference.changed;
        for (const auto& row : difference.rows) {
            if (row.change != "removed")
                continue;
            if (std::ranges::find(req->removals_kept, row.key) != req->removals_kept.end())
                ++response.kept;
            else
                ++response.deleted;
        }
        response.message = std::format("Imported: {} added, {} changed, {} deleted, {} kept.",
                                       response.added,
                                       response.changed,
                                       response.deleted,
                                       response.kept);
        BOOST_LOG_SEV(lg(), info) << "pricing engines imported | corr=" << correlation_id << " "
                                  << response.message;
        reply(nats_, msg, response);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "pricing engines import refused | corr=" << correlation_id
                                  << " " << e.what();
        response.message = e.what();
        reply(nats_, msg, response);
    }
}

}