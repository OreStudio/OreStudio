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
#ifndef ORES_REPORTING_API_MESSAGING_REPORT_CASHFLOW_PROTOCOL_HPP
#define ORES_REPORTING_API_MESSAGING_REPORT_CASHFLOW_PROTOCOL_HPP

#include "ores.reporting.api/domain/report_cashflow_codec.hpp"
#include "ores.reporting.api/export.hpp"
#include "ores.utility/codec/wire_fields.hpp"
#include "ores.utility/codec/wire_traits_fwd.hpp"
#include "ores.utility/domain/protocol.hpp"
#include "ores.utility/domain/protocol_codec.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string>
#include <tuple>
#include <vector>

namespace ores::reporting::messaging {

struct report_cashflow_key {
    boost::uuids::uuid id;

    friend bool operator==(const report_cashflow_key&, const report_cashflow_key&) = default;
};

struct report_cashflow_write {
    boost::uuids::uuid id;
    boost::uuids::uuid party_id;
    boost::uuids::uuid report_instance_id;
    std::string family;
    std::string subject_type;
    std::string subject_id;
    std::string scenario;
    std::string cashflow_no;
    std::string leg_no;
    std::chrono::system_clock::time_point as_of;
    std::string measure;
    std::string part;
    boost::uuids::uuid report_definition_id;
    std::optional<std::chrono::system_clock::time_point> pay_date;
    std::string flow_type;
    std::string currency;
    std::string qualifiers;
    double value;
    std::string source;

    friend bool operator==(const report_cashflow_write&, const report_cashflow_write&) = default;
};

struct report_cashflow_change {
    report_cashflow_write write;
    ores::utility::domain::precondition precondition;

    friend bool operator==(const report_cashflow_change&, const report_cashflow_change&) = default;
};

struct report_cashflow_removal {
    report_cashflow_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;

    friend bool operator==(const report_cashflow_removal&,
                           const report_cashflow_removal&) = default;
};

struct report_cashflow_lookup {
    report_cashflow_key key;
    std::optional<ores::reporting::domain::report_cashflow> report_cashflow;

    friend bool operator==(const report_cashflow_lookup&, const report_cashflow_lookup&) = default;
};

struct report_cashflows_filter {
    std::optional<boost::uuids::uuid> report_instance_id;
    std::optional<std::string> family;
    std::optional<std::string> subject_id;
    std::optional<std::string> measure;
    std::optional<std::string> part;
    std::optional<std::string> flow_type;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> report_instance_id_one_of;
    std::optional<std::vector<std::string>> family_one_of;
    std::optional<std::vector<std::string>> subject_id_one_of;
    std::optional<std::vector<std::string>> measure_one_of;
    std::optional<std::vector<std::string>> part_one_of;
    std::optional<std::vector<std::string>> flow_type_one_of;

    friend bool operator==(const report_cashflows_filter&,
                           const report_cashflows_filter&) = default;
};

struct report_cashflow_event {
    boost::uuids::uuid event_id;
    report_cashflow_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;

    friend bool operator==(const report_cashflow_event&, const report_cashflow_event&) = default;
};

struct list_report_cashflows_request {
    using response_type = struct list_report_cashflows_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_cashflows.list";
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
    std::optional<report_cashflows_filter> filter;
    std::optional<std::string> as_of;

    friend bool operator==(const list_report_cashflows_request&,
                           const list_report_cashflows_request&) = default;
};

struct list_report_cashflows_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_cashflow> cashflows;
    std::uint64_t total;

    friend bool operator==(const list_report_cashflows_response&,
                           const list_report_cashflows_response&) = default;
};

struct get_report_cashflow_request {
    using response_type = struct get_report_cashflow_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_cashflows.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_cashflow_key key;

    friend bool operator==(const get_report_cashflow_request&,
                           const get_report_cashflow_request&) = default;
};

struct get_report_cashflow_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_cashflow> report_cashflow;

    friend bool operator==(const get_report_cashflow_response&,
                           const get_report_cashflow_response&) = default;
};

struct get_many_report_cashflows_request {
    using response_type = struct get_many_report_cashflows_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_cashflows.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_cashflow_key> keys;

    friend bool operator==(const get_many_report_cashflows_request&,
                           const get_many_report_cashflows_request&) = default;
};

struct get_many_report_cashflows_response {
    ores::utility::domain::result result;
    std::vector<report_cashflow_lookup> entries;

    friend bool operator==(const get_many_report_cashflows_response&,
                           const get_many_report_cashflows_response&) = default;
};

struct put_report_cashflow_request {
    using response_type = struct put_report_cashflow_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_cashflows.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_cashflow_change change;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_report_cashflow_request&,
                           const put_report_cashflow_request&) = default;
};

struct put_report_cashflow_response {
    ores::utility::domain::result result;
    std::optional<ores::reporting::domain::report_cashflow> report_cashflow;

    friend bool operator==(const put_report_cashflow_response&,
                           const put_report_cashflow_response&) = default;
};

struct put_many_report_cashflows_request {
    using response_type = struct put_many_report_cashflows_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_cashflows.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_cashflow_change> changes;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const put_many_report_cashflows_request&,
                           const put_many_report_cashflows_request&) = default;
};

struct put_many_report_cashflows_response {
    ores::utility::domain::result result;
    std::vector<ores::reporting::domain::report_cashflow> cashflows;

    friend bool operator==(const put_many_report_cashflows_response&,
                           const put_many_report_cashflows_response&) = default;
};

struct delete_report_cashflow_request {
    using response_type = struct delete_report_cashflow_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_cashflows.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    report_cashflow_removal removal;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_report_cashflow_request&,
                           const delete_report_cashflow_request&) = default;
};

struct delete_report_cashflow_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_report_cashflow_response&,
                           const delete_report_cashflow_response&) = default;
};

struct delete_many_report_cashflows_request {
    using response_type = struct delete_many_report_cashflows_response;
    static constexpr std::string_view nats_subject = "reporting.v1.report_cashflows.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<report_cashflow_removal> removals;
    ores::utility::domain::change_intent intent;

    friend bool operator==(const delete_many_report_cashflows_request&,
                           const delete_many_report_cashflows_request&) = default;
};

struct delete_many_report_cashflows_response {
    ores::utility::domain::result result;

    friend bool operator==(const delete_many_report_cashflows_response&,
                           const delete_many_report_cashflows_response&) = default;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace report_cashflow_event_subjects {
inline constexpr std::string_view created = "reporting.v1.events.report_cashflows.created";
inline constexpr std::string_view updated = "reporting.v1.events.report_cashflows.updated";
inline constexpr std::string_view deleted = "reporting.v1.events.report_cashflows.deleted";
}
/**
 * @brief The members of report_cashflow_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_cashflow_key>) {
    return std::tuple{
        utility::codec::field{"id", &report_cashflow_key::id},
    };
}

/**
 * @brief The members of report_cashflow_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_cashflow_write>) {
    return std::tuple{
        utility::codec::field{"id", &report_cashflow_write::id},
        utility::codec::field{"party_id", &report_cashflow_write::party_id},
        utility::codec::field{"report_instance_id", &report_cashflow_write::report_instance_id},
        utility::codec::field{"family", &report_cashflow_write::family},
        utility::codec::field{"subject_type", &report_cashflow_write::subject_type},
        utility::codec::field{"subject_id", &report_cashflow_write::subject_id},
        utility::codec::field{"scenario", &report_cashflow_write::scenario},
        utility::codec::field{"cashflow_no", &report_cashflow_write::cashflow_no},
        utility::codec::field{"leg_no", &report_cashflow_write::leg_no},
        utility::codec::field{"as_of", &report_cashflow_write::as_of},
        utility::codec::field{"measure", &report_cashflow_write::measure},
        utility::codec::field{"part", &report_cashflow_write::part},
        utility::codec::field{"report_definition_id", &report_cashflow_write::report_definition_id},
        utility::codec::field{"pay_date", &report_cashflow_write::pay_date},
        utility::codec::field{"flow_type", &report_cashflow_write::flow_type},
        utility::codec::field{"currency", &report_cashflow_write::currency},
        utility::codec::field{"qualifiers", &report_cashflow_write::qualifiers},
        utility::codec::field{"value", &report_cashflow_write::value},
        utility::codec::field{"source", &report_cashflow_write::source},
    };
}

/**
 * @brief The members of report_cashflow_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_cashflow_change>) {
    return std::tuple{
        utility::codec::field{"write", &report_cashflow_change::write},
        utility::codec::field{"precondition", &report_cashflow_change::precondition},
    };
}

/**
 * @brief The members of report_cashflow_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_cashflow_removal>) {
    return std::tuple{
        utility::codec::field{"key", &report_cashflow_removal::key},
        utility::codec::field{"precondition", &report_cashflow_removal::precondition},
    };
}

/**
 * @brief The members of report_cashflow_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_cashflow_lookup>) {
    return std::tuple{
        utility::codec::field{"key", &report_cashflow_lookup::key},
        utility::codec::field{"report_cashflow", &report_cashflow_lookup::report_cashflow},
    };
}

/**
 * @brief The members of report_cashflows_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_cashflows_filter>) {
    return std::tuple{
        utility::codec::field{"report_instance_id", &report_cashflows_filter::report_instance_id},
        utility::codec::field{"family", &report_cashflows_filter::family},
        utility::codec::field{"subject_id", &report_cashflows_filter::subject_id},
        utility::codec::field{"measure", &report_cashflows_filter::measure},
        utility::codec::field{"part", &report_cashflows_filter::part},
        utility::codec::field{"flow_type", &report_cashflows_filter::flow_type},
        utility::codec::field{"id_one_of", &report_cashflows_filter::id_one_of},
        utility::codec::field{"report_instance_id_one_of",
                              &report_cashflows_filter::report_instance_id_one_of},
        utility::codec::field{"family_one_of", &report_cashflows_filter::family_one_of},
        utility::codec::field{"subject_id_one_of", &report_cashflows_filter::subject_id_one_of},
        utility::codec::field{"measure_one_of", &report_cashflows_filter::measure_one_of},
        utility::codec::field{"part_one_of", &report_cashflows_filter::part_one_of},
        utility::codec::field{"flow_type_one_of", &report_cashflows_filter::flow_type_one_of},
    };
}

/**
 * @brief The members of report_cashflow_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<report_cashflow_event>) {
    return std::tuple{
        utility::codec::field{"event_id", &report_cashflow_event::event_id},
        utility::codec::field{"key", &report_cashflow_event::key},
        utility::codec::field{"action", &report_cashflow_event::action},
        utility::codec::field{"version", &report_cashflow_event::version},
        utility::codec::field{"occurred_at", &report_cashflow_event::occurred_at},
        utility::codec::field{"correlation_id", &report_cashflow_event::correlation_id},
    };
}

/**
 * @brief The members of list_report_cashflows_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_report_cashflows_request>) {
    return std::tuple{
        utility::codec::field{"offset", &list_report_cashflows_request::offset},
        utility::codec::field{"limit", &list_report_cashflows_request::limit},
        utility::codec::field{"order", &list_report_cashflows_request::order},
        utility::codec::field{"filter", &list_report_cashflows_request::filter},
        utility::codec::field{"as_of", &list_report_cashflows_request::as_of},
    };
}

/**
 * @brief The members of list_report_cashflows_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<list_report_cashflows_response>) {
    return std::tuple{
        utility::codec::field{"result", &list_report_cashflows_response::result},
        utility::codec::field{"cashflows", &list_report_cashflows_response::cashflows},
        utility::codec::field{"total", &list_report_cashflows_response::total},
    };
}

/**
 * @brief The members of get_report_cashflow_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_report_cashflow_request>) {
    return std::tuple{
        utility::codec::field{"key", &get_report_cashflow_request::key},
    };
}

/**
 * @brief The members of get_report_cashflow_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_report_cashflow_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_report_cashflow_response::result},
        utility::codec::field{"report_cashflow", &get_report_cashflow_response::report_cashflow},
    };
}

/**
 * @brief The members of get_many_report_cashflows_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_report_cashflows_request>) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_report_cashflows_request::keys},
    };
}

/**
 * @brief The members of get_many_report_cashflows_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<get_many_report_cashflows_response>) {
    return std::tuple{
        utility::codec::field{"result", &get_many_report_cashflows_response::result},
        utility::codec::field{"entries", &get_many_report_cashflows_response::entries},
    };
}

/**
 * @brief The members of put_report_cashflow_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_report_cashflow_request>) {
    return std::tuple{
        utility::codec::field{"change", &put_report_cashflow_request::change},
        utility::codec::field{"intent", &put_report_cashflow_request::intent},
    };
}

/**
 * @brief The members of put_report_cashflow_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_report_cashflow_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_report_cashflow_response::result},
        utility::codec::field{"report_cashflow", &put_report_cashflow_response::report_cashflow},
    };
}

/**
 * @brief The members of put_many_report_cashflows_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_report_cashflows_request>) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_report_cashflows_request::changes},
        utility::codec::field{"intent", &put_many_report_cashflows_request::intent},
    };
}

/**
 * @brief The members of put_many_report_cashflows_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<put_many_report_cashflows_response>) {
    return std::tuple{
        utility::codec::field{"result", &put_many_report_cashflows_response::result},
        utility::codec::field{"cashflows", &put_many_report_cashflows_response::cashflows},
    };
}

/**
 * @brief The members of delete_report_cashflow_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_report_cashflow_request>) {
    return std::tuple{
        utility::codec::field{"removal", &delete_report_cashflow_request::removal},
        utility::codec::field{"intent", &delete_report_cashflow_request::intent},
    };
}

/**
 * @brief The members of delete_report_cashflow_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_report_cashflow_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_report_cashflow_response::result},
    };
}

/**
 * @brief The members of delete_many_report_cashflows_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(std::type_identity<delete_many_report_cashflows_request>) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_report_cashflows_request::removals},
        utility::codec::field{"intent", &delete_many_report_cashflows_request::intent},
    };
}

/**
 * @brief The members of delete_many_report_cashflows_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto
wire_fields(std::type_identity<delete_many_report_cashflows_response>) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_report_cashflows_response::result},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_cashflow_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_cashflow_key& v);
    static ores::reporting::messaging::report_cashflow_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_cashflow_key& v);
    static ores::reporting::messaging::report_cashflow_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_cashflow_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_cashflow_write& v);
    static ores::reporting::messaging::report_cashflow_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_cashflow_write& v);
    static ores::reporting::messaging::report_cashflow_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_cashflow_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_cashflow_change& v);
    static ores::reporting::messaging::report_cashflow_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_cashflow_change& v);
    static ores::reporting::messaging::report_cashflow_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_cashflow_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_cashflow_removal& v);
    static ores::reporting::messaging::report_cashflow_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_cashflow_removal& v);
    static ores::reporting::messaging::report_cashflow_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_cashflow_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_cashflow_lookup& v);
    static ores::reporting::messaging::report_cashflow_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_cashflow_lookup& v);
    static ores::reporting::messaging::report_cashflow_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_cashflows_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_cashflows_filter& v);
    static ores::reporting::messaging::report_cashflows_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_cashflows_filter& v);
    static ores::reporting::messaging::report_cashflows_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT wire_traits<ores::reporting::messaging::report_cashflow_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::report_cashflow_event& v);
    static ores::reporting::messaging::report_cashflow_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::reporting::messaging::report_cashflow_event& v);
    static ores::reporting::messaging::report_cashflow_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_cashflows_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_cashflows_request& v);
    static ores::reporting::messaging::list_report_cashflows_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_cashflows_request& v);
    static ores::reporting::messaging::list_report_cashflows_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::list_report_cashflows_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::list_report_cashflows_response& v);
    static ores::reporting::messaging::list_report_cashflows_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::list_report_cashflows_response& v);
    static ores::reporting::messaging::list_report_cashflows_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_cashflow_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::get_report_cashflow_request& v);
    static ores::reporting::messaging::get_report_cashflow_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_cashflow_request& v);
    static ores::reporting::messaging::get_report_cashflow_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_report_cashflow_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_report_cashflow_response& v);
    static ores::reporting::messaging::get_report_cashflow_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_report_cashflow_response& v);
    static ores::reporting::messaging::get_report_cashflow_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_cashflows_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_cashflows_request& v);
    static ores::reporting::messaging::get_many_report_cashflows_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_cashflows_request& v);
    static ores::reporting::messaging::get_many_report_cashflows_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::get_many_report_cashflows_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::get_many_report_cashflows_response& v);
    static ores::reporting::messaging::get_many_report_cashflows_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::get_many_report_cashflows_response& v);
    static ores::reporting::messaging::get_many_report_cashflows_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_cashflow_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::reporting::messaging::put_report_cashflow_request& v);
    static ores::reporting::messaging::put_report_cashflow_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_cashflow_request& v);
    static ores::reporting::messaging::put_report_cashflow_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_report_cashflow_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_report_cashflow_response& v);
    static ores::reporting::messaging::put_report_cashflow_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_report_cashflow_response& v);
    static ores::reporting::messaging::put_report_cashflow_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_cashflows_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_cashflows_request& v);
    static ores::reporting::messaging::put_many_report_cashflows_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_cashflows_request& v);
    static ores::reporting::messaging::put_many_report_cashflows_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::put_many_report_cashflows_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::put_many_report_cashflows_response& v);
    static ores::reporting::messaging::put_many_report_cashflows_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::put_many_report_cashflows_response& v);
    static ores::reporting::messaging::put_many_report_cashflows_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_cashflow_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_cashflow_request& v);
    static ores::reporting::messaging::delete_report_cashflow_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_cashflow_request& v);
    static ores::reporting::messaging::delete_report_cashflow_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_report_cashflow_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_report_cashflow_response& v);
    static ores::reporting::messaging::delete_report_cashflow_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_report_cashflow_response& v);
    static ores::reporting::messaging::delete_report_cashflow_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_cashflows_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_cashflows_request& v);
    static ores::reporting::messaging::delete_many_report_cashflows_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_cashflows_request& v);
    static ores::reporting::messaging::delete_many_report_cashflows_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REPORTING_API_EXPORT
    wire_traits<ores::reporting::messaging::delete_many_report_cashflows_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::reporting::messaging::delete_many_report_cashflows_response& v);
    static ores::reporting::messaging::delete_many_report_cashflows_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::reporting::messaging::delete_many_report_cashflows_response& v);
    static ores::reporting::messaging::delete_many_report_cashflows_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
