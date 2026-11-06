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
#ifndef ORES_DQ_API_MESSAGING_REPORT_DEFINITION_TEMPLATE_PROTOCOL_HPP
#define ORES_DQ_API_MESSAGING_REPORT_DEFINITION_TEMPLATE_PROTOCOL_HPP

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
 * @brief One report definition offered as a template.
 */
struct dq_report_definition_template {
    /**
     * @brief The template's name.
     */
    std::string name;
    /**
     * @brief What the template produces.
     */
    std::string description;
    /**
     * @brief The kind of report the template instantiates.
     */
    std::string report_type;
    /**
     * @brief The schedule the template suggests.
     */
    std::string schedule_expression;
    /**
     * @brief How overlapping runs of the template are treated.
     */
    std::string concurrency_policy;
    /**
     * @brief Where the template sits in the list.
     */
    int display_order = 0;

    friend bool operator==(const dq_report_definition_template&,
                           const dq_report_definition_template&) = default;
};

/**
 * @brief Asks for the templates one dataset bundle offers.
 */
struct list_dq_report_definition_templates_request {
    using response_type = struct list_dq_report_definition_templates_response;
    static constexpr std::string_view nats_subject = "dq.v1.report_definition_templates.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    /**
     * @brief The bundle whose templates are listed.
     */
    std::string bundle_code = "risk_management";

    friend bool operator==(const list_dq_report_definition_templates_request&,
                           const list_dq_report_definition_templates_request&) = default;
};

/**
 * @brief Reports the templates, or why they could not be read.
 */
struct list_dq_report_definition_templates_response {
    /**
     * @brief Whether the read completed.
     */
    bool success = false;
    /**
     * @brief Why it failed, when it did.
     */
    std::string message;
    /**
     * @brief The templates, in display order.
     */
    std::vector<dq_report_definition_template> templates;

    friend bool operator==(const list_dq_report_definition_templates_response&,
                           const list_dq_report_definition_templates_response&) = default;
};

/**
 * @brief The members of dq_report_definition_template, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<dq_report_definition_template>) {
    return std::tuple{
        ::ores::utility::codec::field{"name", &dq_report_definition_template::name},
        ::ores::utility::codec::field{"description", &dq_report_definition_template::description},
        ::ores::utility::codec::field{"report_type", &dq_report_definition_template::report_type},
        ::ores::utility::codec::field{"schedule_expression",
                                      &dq_report_definition_template::schedule_expression},
        ::ores::utility::codec::field{"concurrency_policy",
                                      &dq_report_definition_template::concurrency_policy},
        ::ores::utility::codec::field{"display_order",
                                      &dq_report_definition_template::display_order},
    };
}

/**
 * @brief The members of list_dq_report_definition_templates_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_dq_report_definition_templates_request>) {
    return std::tuple{
        ::ores::utility::codec::field{"bundle_code",
                                      &list_dq_report_definition_templates_request::bundle_code},
    };
}

/**
 * @brief The members of list_dq_report_definition_templates_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<list_dq_report_definition_templates_response>) {
    return std::tuple{
        ::ores::utility::codec::field{"success",
                                      &list_dq_report_definition_templates_response::success},
        ::ores::utility::codec::field{"message",
                                      &list_dq_report_definition_templates_response::message},
        ::ores::utility::codec::field{"templates",
                                      &list_dq_report_definition_templates_response::templates},
    };
}

}
namespace ores::utility::codec {

template <>
struct ORES_DQ_API_EXPORT wire_traits<ores::dq::messaging::dq_report_definition_template> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::dq::messaging::dq_report_definition_template& v);
    static ores::dq::messaging::dq_report_definition_template read_json(std::string_view text);
    static std::string write_msgpack(const ores::dq::messaging::dq_report_definition_template& v);
    static ores::dq::messaging::dq_report_definition_template
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::list_dq_report_definition_templates_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_dq_report_definition_templates_request& v);
    static ores::dq::messaging::list_dq_report_definition_templates_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_dq_report_definition_templates_request& v);
    static ores::dq::messaging::list_dq_report_definition_templates_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_DQ_API_EXPORT
    wire_traits<ores::dq::messaging::list_dq_report_definition_templates_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::dq::messaging::list_dq_report_definition_templates_response& v);
    static ores::dq::messaging::list_dq_report_definition_templates_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::dq::messaging::list_dq_report_definition_templates_response& v);
    static ores::dq::messaging::list_dq_report_definition_templates_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
