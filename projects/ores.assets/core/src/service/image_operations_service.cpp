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
#include "ores.assets.core/service/image_operations_service.hpp"
#include "ores.assets.core/validation/image_upload_validator.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.service/messaging/handler_helpers.hpp"
#include "ores.utility/convert/base64_converter.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <cstdint>
#include <exception>
#include <string>
#include <utility>
#include <vector>

using ores::service::messaging::stamp;

namespace ores::assets::service {

using namespace ores::logging;

namespace {

ores::utility::domain::result refusal_result(const validation::image_upload_refusal& refusal) {
    ores::utility::domain::result result;
    result.outcome = ores::utility::domain::outcome::invalid;
    result.code = refusal.code;
    result.message = refusal.message;
    result.fields.push_back(ores::utility::domain::field_failure{
        .field = refusal.field, .code = refusal.code, .message = refusal.message});
    return result;
}

}

image_operations_service::image_operations_service(context ctx)
    : ctx_(std::move(ctx)) {}

messaging::upload_image_response
image_operations_service::upload_image(const messaging::upload_image_request& request) {
    messaging::upload_image_response response;
    std::vector<std::uint8_t> bytes;
    try {
        bytes = ores::utility::convert::base64_converter::convert(request.data);
    } catch (const std::exception& e) {
        BOOST_LOG_SEV(lg(), warn) << "Refused an upload: the data is not readable base64: "
                                  << e.what();
        response.result = refusal_result(
            validation::image_upload_refusal{.code = "invalid_image",
                                             .field = "data",
                                             .message = "The image data is not readable base64."});
        return response;
    }
    if (const auto refusal =
            validation::image_upload_validator::validate(request.mime_type, bytes)) {
        BOOST_LOG_SEV(lg(), warn) << "Refused an upload: " << refusal->code << " ("
                                  << refusal->field << ")";
        response.result = refusal_result(*refusal);
        return response;
    }
    domain::image value;
    value.id = uuid_generator_();
    value.code = boost::uuids::to_string(value.id);
    value.mime_type = request.mime_type;
    value.data = std::move(bytes);
    stamp(value, ctx_);
    repo_.write(ctx_, value);
    response.image_id = boost::uuids::to_string(value.id);
    BOOST_LOG_SEV(lg(), debug) << "Stored an uploaded image. id: " << response.image_id;
    return response;
}

messaging::get_image_upload_policy_response image_operations_service::get_image_upload_policy(
    const messaging::get_image_upload_policy_request&) {
    const auto rules = validation::image_upload_validator::policy();
    messaging::get_image_upload_policy_response response;
    response.formats = rules.formats;
    response.max_size_bytes = static_cast<int>(rules.max_size_bytes);
    response.min_width = static_cast<int>(rules.min_width);
    response.min_height = static_cast<int>(rules.min_height);
    return response;
}

messaging::ensure_image_response
image_operations_service::ensure_image(const messaging::ensure_image_request& request) {
    messaging::ensure_image_response response;

    // A tenant that already holds the code is answered with what it holds, so a
    // repeated reconcile writes nothing.
    const auto existing = repo_.read_latest_by_code(ctx_, request.code);
    if (!existing.empty()) {
        response.image_id = boost::uuids::to_string(existing.front().id);
        BOOST_LOG_SEV(lg(), debug) << "The tenant already holds image '" << request.code << "'.";
        return response;
    }

    // The system tenant carries the templates an installation ships. Reading it
    // is a cross-tenant read, so it goes through the function that owns that
    // read rather than the tenant-scoped repository.
    const auto rows = ores::database::repository::execute_parameterized_multi_column_query(
        ctx_,
        "SELECT description, mime_type, data FROM ores_assets_get_template_image_fn($1)",
        {request.code},
        lg(),
        "Reading a template image");

    if (rows.empty() || rows.front().size() < 3 || !rows.front()[0] || !rows.front()[1] ||
        !rows.front()[2]) {
        BOOST_LOG_SEV(lg(), warn) << "No template image with code '" << request.code << "'.";
        response.result.outcome = ores::utility::domain::outcome::invalid;
        response.result.code = "template_not_found";
        response.result.message =
            "The installation carries no template image with the code '" + request.code + "'.";
        return response;
    }

    domain::image value;
    value.id = uuid_generator_();
    value.code = request.code;
    value.description = *rows.front()[0];
    value.mime_type = *rows.front()[1];
    value.data = ores::utility::convert::base64_converter::convert(*rows.front()[2]);
    stamp(value, ctx_);
    repo_.write(ctx_, value);
    response.image_id = boost::uuids::to_string(value.id);
    BOOST_LOG_SEV(lg(), debug) << "Copied the template image '" << request.code
                               << "' into the tenant: " << response.image_id;
    return response;
}

}
