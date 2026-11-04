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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_TEXT_VALUE_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_TEXT_VALUE_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_text_value.hpp"
#include "ores.reporting.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::reporting::messaging {

struct report_text_value_key {
    boost::uuids::uuid id;

    friend bool operator==(const report_text_value_key&, const report_text_value_key&) = default;
};

struct report_text_value_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid report_instance_id;
    boost::uuids::uuid result_id;
    std::string source;
    int line;
    std::chrono::system_clock::time_point as_of;
    boost::uuids::uuid report_definition_id;
    std::string family;
    std::string subject_type;
    std::string subject_id;
    std::string name;
    std::string value_type;
    std::string value;
    std::optional<double> figure;

    friend bool operator==(const report_text_value_write&,
                           const report_text_value_write&) = default;
};

struct report_text_value_change {
    report_text_value_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const report_text_value_change&,
                           const report_text_value_change&) = default;
};

struct report_text_value_removal {
    report_text_value_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const report_text_value_removal&,
                           const report_text_value_removal&) = default;
};

struct report_text_value_lookup {
    report_text_value_key key;
    std::optional<ores::reporting::domain::report_text_value> report_text_value;

    friend bool operator==(const report_text_value_lookup&,
                           const report_text_value_lookup&) = default;
};

struct report_text_values_filter {
    std::optional<boost::uuids::uuid> report_instance_id;
    std::optional<std::string> family;
    std::optional<std::string> subject_id;
    std::optional<std::string> name;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_instance_id_one_of;
    std::optional<std::vector<std::string>> family_one_of;
    std::optional<std::vector<std::string>> subject_id_one_of;
    std::optional<std::vector<std::string>> name_one_of;

    friend bool operator==(const report_text_values_filter&,
                           const report_text_values_filter&) = default;
};

struct report_text_value_event {
    boost::uuids::uuid event_id;
    report_text_value_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const report_text_value_event&,
                           const report_text_value_event&) = default;
};

struct list_report_text_values_request {
    using response_type = struct list_report_text_values_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_text_values.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<report_text_values_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_report_text_values_request&,
                           const list_report_text_values_request&) = default;
};

struct list_report_text_values_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_text_value> values;
    std::uint64_t total;

    friend bool operator==(const list_report_text_values_response&,
                           const list_report_text_values_response&) = default;
};

struct get_report_text_value_request {
    using response_type = struct get_report_text_value_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_text_values.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_text_value_key key;

    friend bool operator==(const get_report_text_value_request&,
                           const get_report_text_value_request&) = default;
};

struct get_report_text_value_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_text_value> report_text_value;

    friend bool operator==(const get_report_text_value_response&,
                           const get_report_text_value_response&) = default;
};

struct get_many_report_text_values_request {
    using response_type = struct get_many_report_text_values_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_text_values.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_text_value_key> keys;

    friend bool operator==(const get_many_report_text_values_request&,
                           const get_many_report_text_values_request&) = default;
};

struct get_many_report_text_values_response {
    ores::utility::domain::result result;
    std::vector<report_text_value_lookup> entries;

    friend bool operator==(const get_many_report_text_values_response&,
                           const get_many_report_text_values_response&) = default;
};

struct put_report_text_value_request {
    using response_type = struct put_report_text_value_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_text_values.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_text_value_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_report_text_value_request&,
                           const put_report_text_value_request&) = default;
};

struct put_report_text_value_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_text_value> report_text_value;

    friend bool operator==(const put_report_text_value_response&,
                           const put_report_text_value_response&) = default;
};

struct put_many_report_text_values_request {
    using response_type = struct put_many_report_text_values_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_text_values.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_text_value_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_report_text_values_request&,
                           const put_many_report_text_values_request&) = default;
};

struct put_many_report_text_values_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_text_value> values;

    friend bool operator==(const put_many_report_text_values_response&,
                           const put_many_report_text_values_response&) = default;
};

struct delete_report_text_value_request {
    using response_type = struct delete_report_text_value_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_text_values.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_text_value_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_report_text_value_request&,
                           const delete_report_text_value_request&) = default;
};

struct delete_report_text_value_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_report_text_value_response&,
                           const delete_report_text_value_response&) = default;
};

struct delete_many_report_text_values_request {
    using response_type = struct delete_many_report_text_values_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_text_values.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_text_value_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_report_text_values_request&,
                           const delete_many_report_text_values_request&) = default;
};

struct delete_many_report_text_values_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_report_text_values_response&,
                           const delete_many_report_text_values_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_text_value_event_subjects {
inline constexpr std::string_view created = "reporting.v1.events.report_text_values.created";
inline constexpr std::string_view updated = "reporting.v1.events.report_text_values.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.report_text_values.deleted";
}
/**
 * @brief The members of report_text_value_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_text_value_key>) {
    return std::tuple{
        utility::codec::field{"id", &report_text_value_key::id},
    };
}

/**
 * @brief The members of report_text_value_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_text_value_write>) {
    return std::tuple{
        utility::codec::field{"id", &report_text_value_write::id},
        utility::codec::field{"party_id", &report_text_value_write::party_id},
        utility::codec::field{"report_instance_id", &report_text_value_write::report_instance_id},
        utility::codec::field{"result_id", &report_text_value_write::result_id},
        utility::codec::field{"source", &report_text_value_write::source},
        utility::codec::field{"line", &report_text_value_write::line},
        utility::codec::field{"as_of", &report_text_value_write::as_of},
        utility::codec::field{"report_definition_id",
                              &report_text_value_write::report_definition_id},
        utility::codec::field{"family", &report_text_value_write::family},
        utility::codec::field{"subject_type", &report_text_value_write::subject_type},
        utility::codec::field{"subject_id", &report_text_value_write::subject_id},
        utility::codec::field{"name", &report_text_value_write::name},
        utility::codec::field{"value_type", &report_text_value_write::value_type},
        utility::codec::field{"value", &report_text_value_write::value},
        utility::codec::field{"figure", &report_text_value_write::figure},
    };
}

/**
 * @brief The members of report_text_value_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_text_value_change>) {
    return std::tuple{
        utility::codec::field{"write", &report_text_value_change::write},
        utility::codec::field{"precondition", &report_text_value_change::precondition},
    };
}

/**
 * @brief The members of report_text_value_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_text_value_removal>) {
    return std::tuple{
        utility::codec::field{"key", &report_text_value_removal::key},
        utility::codec::field{"precondition", &report_text_value_removal::precondition},
    };
}

/**
 * @brief The members of report_text_value_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_text_value_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &report_text_value_lookup::key},
        utility::codec::field{"report_text_value", &report_text_value_lookup::report_text_value},
    };
}

/**
 * @brief The members of report_text_values_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_text_values_filter>) {
    return std::tuple{
        utility::codec::field{"report_instance_id", &report_text_values_filter::report_instance_id},
        utility::codec::field{"family", &report_text_values_filter::family},
        utility::codec::field{"subject_id", &report_text_values_filter::subject_id},
        utility::codec::field{"name", &report_text_values_filter::name},
        utility::codec::field{"id_one_of", &report_text_values_filter::id_one_of},
        utility::codec::field{"report_instance_id_one_of",
                              &report_text_values_filter::report_instance_id_one_of},
        utility::codec::field{"family_one_of", &report_text_values_filter::family_one_of},
        utility::codec::field{"subject_id_one_of", &report_text_values_filter::subject_id_one_of},
        utility::codec::field{"name_one_of", &report_text_values_filter::name_one_of},
    };
}

/**
 * @brief The members of report_text_value_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_text_value_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &report_text_value_event::event_id},
        utility::codec::field{"key", &report_text_value_event::key},
        utility::codec::field{"action", &report_text_value_event::action},
        utility::codec::field{"version", &report_text_value_event::version},
        utility::codec::field{"occurred_at", &report_text_value_event::occurred_at},
        utility::codec::field{"correlation_id", &report_text_value_event::correlation_id},
    };
}

/**
 * @brief The members of list_report_text_values_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_report_text_values_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_report_text_values_request::offset},
        utility::codec::field{"limit", &list_report_text_values_request::limit},
        utility::codec::field{"order", &list_report_text_values_request::order},
        utility::codec::field{"filter", &list_report_text_values_request::filter},
        utility::codec::field{"as_of", &list_report_text_values_request::as_of},
    };
}

/**
 * @brief The members of list_report_text_values_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_report_text_values_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_report_text_values_response::result},
        utility::codec::field{"values", &list_report_text_values_response::values},
        utility::codec::field{"total", &list_report_text_values_response::total},
    };
}

/**
 * @brief The members of get_report_text_value_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_report_text_value_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_report_text_value_request::key},
    };
}

/**
 * @brief The members of get_report_text_value_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_report_text_value_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_report_text_value_response::result},
        utility::codec::field{"report_text_value",
                              &get_report_text_value_response::report_text_value},
    };
}

/**
 * @brief The members of get_many_report_text_values_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_report_text_values_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_report_text_values_request::keys},
    };
}

/**
 * @brief The members of get_many_report_text_values_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_report_text_values_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_report_text_values_response::result},
        utility::codec::field{"entries", &get_many_report_text_values_response::entries},
    };
}

/**
 * @brief The members of put_report_text_value_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_report_text_value_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_report_text_value_request::change},
        utility::codec::field{"intent", &put_report_text_value_request::intent},
    };
}

/**
 * @brief The members of put_report_text_value_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_report_text_value_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_report_text_value_response::result},
        utility::codec::field{"report_text_value",
                              &put_report_text_value_response::report_text_value},
    };
}

/**
 * @brief The members of put_many_report_text_values_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_report_text_values_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_report_text_values_request::changes},
        utility::codec::field{"intent", &put_many_report_text_values_request::intent},
    };
}

/**
 * @brief The members of put_many_report_text_values_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_report_text_values_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_report_text_values_response::result},
        utility::codec::field{"values", &put_many_report_text_values_response::values},
    };
}

/**
 * @brief The members of delete_report_text_value_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_report_text_value_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_report_text_value_request::removal},
        utility::codec::field{"intent", &delete_report_text_value_request::intent},
    };
}

/**
 * @brief The members of delete_report_text_value_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_report_text_value_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_report_text_value_response::result},
    };
}

/**
 * @brief The members of delete_many_report_text_values_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_report_text_values_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_report_text_values_request::removals},
        utility::codec::field{"intent", &delete_many_report_text_values_request::intent},
    };
}

/**
 * @brief The members of delete_many_report_text_values_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_report_text_values_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_report_text_values_response::result},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_text_value_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_text_value_key& v);
    static ores::reporting::messaging::report_text_value_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_text_value_key& v);
    static ores::reporting::messaging::report_text_value_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_text_value_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_text_value_write& v);
    static ores::reporting::messaging::report_text_value_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_text_value_write& v);
    static ores::reporting::messaging::report_text_value_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_text_value_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_text_value_change& v);
    static ores::reporting::messaging::report_text_value_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_text_value_change& v);
    static ores::reporting::messaging::report_text_value_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_text_value_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_text_value_removal& v);
    static ores::reporting::messaging::report_text_value_removal read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_text_value_removal& v);
    static ores::reporting::messaging::report_text_value_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_text_value_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_text_value_lookup& v);
    static ores::reporting::messaging::report_text_value_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_text_value_lookup& v);
    static ores::reporting::messaging::report_text_value_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::report_text_values_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_text_values_filter& v);
    static ores::reporting::messaging::report_text_values_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::report_text_values_filter& v);
    static ores::reporting::messaging::report_text_values_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_text_value_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_text_value_event& v);
    static ores::reporting::messaging::report_text_value_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_text_value_event& v);
    static ores::reporting::messaging::report_text_value_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_text_values_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_text_values_request& v);
    static ores::reporting::messaging::list_report_text_values_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_text_values_request& v);
    static ores::reporting::messaging::list_report_text_values_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_text_values_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_text_values_response& v);
    static ores::reporting::messaging::list_report_text_values_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_text_values_response& v);
    static ores::reporting::messaging::list_report_text_values_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_text_value_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_text_value_request& v);
    static ores::reporting::messaging::get_report_text_value_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_text_value_request& v);
    static ores::reporting::messaging::get_report_text_value_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_text_value_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_text_value_response& v);
    static ores::reporting::messaging::get_report_text_value_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_text_value_response& v);
    static ores::reporting::messaging::get_report_text_value_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_text_values_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_text_values_request& v);
    static ores::reporting::messaging::get_many_report_text_values_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_text_values_request& v);
    static ores::reporting::messaging::get_many_report_text_values_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_text_values_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_text_values_response& v);
    static ores::reporting::messaging::get_many_report_text_values_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_text_values_response& v);
    static ores::reporting::messaging::get_many_report_text_values_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_text_value_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_report_text_value_request& v);
    static ores::reporting::messaging::put_report_text_value_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_text_value_request& v);
    static ores::reporting::messaging::put_report_text_value_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_text_value_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_report_text_value_response& v);
    static ores::reporting::messaging::put_report_text_value_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_text_value_response& v);
    static ores::reporting::messaging::put_report_text_value_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_text_values_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_text_values_request& v);
    static ores::reporting::messaging::put_many_report_text_values_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_text_values_request& v);
    static ores::reporting::messaging::put_many_report_text_values_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_text_values_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_text_values_response& v);
    static ores::reporting::messaging::put_many_report_text_values_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_text_values_response& v);
    static ores::reporting::messaging::put_many_report_text_values_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_text_value_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_text_value_request& v);
    static ores::reporting::messaging::delete_report_text_value_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_text_value_request& v);
    static ores::reporting::messaging::delete_report_text_value_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_text_value_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_text_value_response& v);
    static ores::reporting::messaging::delete_report_text_value_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_text_value_response& v);
    static ores::reporting::messaging::delete_report_text_value_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_text_values_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_text_values_request& v);
    static ores::reporting::messaging::delete_many_report_text_values_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_text_values_request& v);
    static ores::reporting::messaging::delete_many_report_text_values_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_text_values_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_text_values_response& v);
    static ores::reporting::messaging::delete_many_report_text_values_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_text_values_response& v);
    static ores::reporting::messaging::delete_many_report_text_values_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
