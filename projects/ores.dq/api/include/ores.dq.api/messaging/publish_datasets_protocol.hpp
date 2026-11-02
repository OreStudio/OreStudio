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
/**
 * AUTO-GENERATED FILE - DO NOT EDIT MANUALLY
 * Template: cpp_protocol.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_DQ_API_MESSAGING_PUBLISH_DATASETS_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_PUBLISH_DATASETS_PROTOCOL_HPP

#include "ores.dq.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include <cstddef>
#include <span>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace ores::dq::messaging {

/**
 * @brief Asks for a set of datasets to be published.
 */
struct publish_datasets_request {
    using response_type = struct publish_datasets_response;
    static constexpr std::string_view nats_subject = "dq.v1.ops.publish_datasets";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The datasets to publish.
     */
    std::vector<std::string> dataset_ids;
    /**
     * @brief How records are written to the target tables.
     */
    std::string mode = "upsert";
    /**
     * @brief Who asked for the publication.
     */
    std::string published_by;
    /**
     * @brief Whether a dataset's dependencies are published with it.
     */
    bool resolve_dependencies = true;

    friend bool operator==(const publish_datasets_request&,
                           const publish_datasets_request&) = default;
};

/**
 * @brief Reports what the publication dispatched.
 */
struct publish_datasets_response {
    /**
     * @brief Whether the publication was accepted.
     */
    bool success = false;
    /**
     * @brief Why it was refused, when it was.
     */
    std::string message;
    /**
     * @brief The workflow instance the publication started.
     */
    std::string instance_id;
    /**
     * @brief How many datasets were dispatched.
     */
    int datasets_dispatched = 0;

    friend bool operator==(const publish_datasets_response&,
                           const publish_datasets_response&) = default;
};

/**
 * @brief The members of publish_datasets_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const publish_datasets_request&) {
    return std::tuple{
        ::ores::utility::codec::field{"dataset_ids", &publish_datasets_request::dataset_ids},
        ::ores::utility::codec::field{"mode", &publish_datasets_request::mode},
        ::ores::utility::codec::field{"published_by", &publish_datasets_request::published_by},
        ::ores::utility::codec::field{"resolve_dependencies",
                                      &publish_datasets_request::resolve_dependencies},
    };
}

/**
 * @brief The members of publish_datasets_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const publish_datasets_response&) {
    return std::tuple{
        ::ores::utility::codec::field{"success", &publish_datasets_response::success},
        ::ores::utility::codec::field{"message", &publish_datasets_response::message},
        ::ores::utility::codec::field{"instance_id", &publish_datasets_response::instance_id},
        ::ores::utility::codec::field{"datasets_dispatched",
                                      &publish_datasets_response::datasets_dispatched},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::publish_datasets_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::publish_datasets_request& v);
    static ores::dq::messaging::publish_datasets_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::publish_datasets_request& v);
    static ores::dq::messaging::publish_datasets_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::publish_datasets_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::publish_datasets_response& v);
    static ores::dq::messaging::publish_datasets_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::publish_datasets_response& v);
    static ores::dq::messaging::publish_datasets_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
