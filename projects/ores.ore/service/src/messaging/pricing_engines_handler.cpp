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
#include "ores.nats/domain/correlation.hpp"
#include "ores.ore.api/messaging/pricing_engines_protocol.hpp"
#include "ores.ore.core/xml/exporter.hpp"
#include "ores.ore.service/messaging/nats_call.hpp"
#include "ores.reporting.api/domain/file_type_lookup.hpp"
#include "ores.reporting.api/messaging/file_type_protocol.hpp"
#include "ores.service/error_code.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.service/service/request_context.hpp"
#include <cstdint>
#include <format>
#include <optional>
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

/// The most rows one page of the file types read asks for.
constexpr std::uint32_t page = 500;

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

}
