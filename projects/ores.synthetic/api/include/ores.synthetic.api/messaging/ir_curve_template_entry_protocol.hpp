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
#ifndef ORES_SYNTHETIC_API_MESSAGING_IR_CURVE_TEMPLATE_ENTRY_PROTOCOL_HPP
#define ORES_SYNTHETIC_API_MESSAGING_IR_CURVE_TEMPLATE_ENTRY_PROTOCOL_HPP

#include "ores.synthetic.api/domain/ir_curve_template_entry.hpp"
#include "ores.synthetic.api/export.hpp"
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

namespace ores::synthetic::messaging {

struct ir_curve_template_entry_key {
    boost::uuids::uuid id;
};

struct ir_curve_template_entry_write {
    boost::uuids::uuid id;
    boost::uuids::uuid ir_curve_config_id;
    int sequence_index;
    std::string start_tenor_code;
    std::string end_tenor_code;
    std::string instrument_code;
};

struct ir_curve_template_entry_change {
    ir_curve_template_entry_write write;
    ores::utility::domain::precondition precondition;
};

struct ir_curve_template_entry_removal {
    ir_curve_template_entry_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct ir_curve_template_entry_lookup {
    ir_curve_template_entry_key key;
    std::optional<ores::synthetic::domain::ir_curve_template_entry> ir_curve_template_entry;
};

struct ir_curve_template_entries_filter {
    std::optional<boost::uuids::uuid> ir_curve_config_id;
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
    std::optional<std::vector<boost::uuids::uuid>> ir_curve_config_id_one_of;
};

struct ir_curve_template_entry_event {
    boost::uuids::uuid event_id;
    ir_curve_template_entry_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct ir_curve_template_entry_version_key {
    ir_curve_template_entry_key ir_curve_template_entry;
    std::uint32_t version;
};

struct ir_curve_template_entry_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_ir_curve_template_entries_request {
    using response_type = struct list_ir_curve_template_entries_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ir_curve_template_entries.list";
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
    std::optional<ir_curve_template_entries_filter> filter;
    std::optional<std::string> as_of;
};

struct list_ir_curve_template_entries_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::ir_curve_template_entry> ir_curve_template_entries;
    std::uint64_t total;
};

struct get_ir_curve_template_entry_request {
    using response_type = struct get_ir_curve_template_entry_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ir_curve_template_entries.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ir_curve_template_entry_key key;
};

struct get_ir_curve_template_entry_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::ir_curve_template_entry> ir_curve_template_entry;
};

struct get_many_ir_curve_template_entries_request {
    using response_type = struct get_many_ir_curve_template_entries_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_template_entries.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ir_curve_template_entry_key> keys;
};

struct get_many_ir_curve_template_entries_response {
    ores::utility::domain::result result;
    std::vector<ir_curve_template_entry_lookup> entries;
};

struct put_ir_curve_template_entry_request {
    using response_type = struct put_ir_curve_template_entry_response;
    static constexpr std::string_view nats_subject = "synthetic.v1.ir_curve_template_entries.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ir_curve_template_entry_change change;
    ores::utility::domain::change_intent intent;
};

struct put_ir_curve_template_entry_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::ir_curve_template_entry> ir_curve_template_entry;
};

struct put_many_ir_curve_template_entries_request {
    using response_type = struct put_many_ir_curve_template_entries_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_template_entries.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ir_curve_template_entry_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_ir_curve_template_entries_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::ir_curve_template_entry> ir_curve_template_entries;
};

struct delete_ir_curve_template_entry_request {
    using response_type = struct delete_ir_curve_template_entry_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_template_entries.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ir_curve_template_entry_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_ir_curve_template_entry_response {
    ores::utility::domain::result result;
};

struct delete_many_ir_curve_template_entries_request {
    using response_type = struct delete_many_ir_curve_template_entries_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_template_entries.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<ir_curve_template_entry_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_ir_curve_template_entries_response {
    ores::utility::domain::result result;
};

struct list_ir_curve_template_entry_versions_request {
    using response_type = struct list_ir_curve_template_entry_versions_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_template_entries_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ir_curve_template_entry_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<ir_curve_template_entry_versions_filter> filter;
};

struct list_ir_curve_template_entry_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::synthetic::domain::ir_curve_template_entry> versions;
    std::uint64_t total;
};

struct get_ir_curve_template_entry_version_request {
    using response_type = struct get_ir_curve_template_entry_version_response;
    static constexpr std::string_view nats_subject =
        "synthetic.v1.ir_curve_template_entries_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ir_curve_template_entry_version_key key;
};

struct get_ir_curve_template_entry_version_response {
    ores::utility::domain::result result;
    std::optional<ores::synthetic::domain::ir_curve_template_entry> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace ir_curve_template_entry_event_subjects {
inline constexpr std::string_view created = "synthetic.v1.ir_curve_template_entries_events.created";
inline constexpr std::string_view updated = "synthetic.v1.ir_curve_template_entries_events.updated";
inline constexpr std::string_view deleted = "synthetic.v1.ir_curve_template_entries_events.deleted";
}
/**
 * @brief The members of ir_curve_template_entry_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ir_curve_template_entry_key&) {
    return std::tuple{
        utility::codec::field{"id", &ir_curve_template_entry_key::id},
    };
}

/**
 * @brief The members of ir_curve_template_entry_write, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ir_curve_template_entry_write&) {
    return std::tuple{
        utility::codec::field{"id", &ir_curve_template_entry_write::id},
        utility::codec::field{"ir_curve_config_id",
                              &ir_curve_template_entry_write::ir_curve_config_id},
        utility::codec::field{"sequence_index", &ir_curve_template_entry_write::sequence_index},
        utility::codec::field{"start_tenor_code", &ir_curve_template_entry_write::start_tenor_code},
        utility::codec::field{"end_tenor_code", &ir_curve_template_entry_write::end_tenor_code},
        utility::codec::field{"instrument_code", &ir_curve_template_entry_write::instrument_code},
    };
}

/**
 * @brief The members of ir_curve_template_entry_change, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ir_curve_template_entry_change&) {
    return std::tuple{
        utility::codec::field{"write", &ir_curve_template_entry_change::write},
        utility::codec::field{"precondition", &ir_curve_template_entry_change::precondition},
    };
}

/**
 * @brief The members of ir_curve_template_entry_removal, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ir_curve_template_entry_removal&) {
    return std::tuple{
        utility::codec::field{"key", &ir_curve_template_entry_removal::key},
        utility::codec::field{"precondition", &ir_curve_template_entry_removal::precondition},
    };
}

/**
 * @brief The members of ir_curve_template_entry_lookup, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ir_curve_template_entry_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &ir_curve_template_entry_lookup::key},
        utility::codec::field{"ir_curve_template_entry",
                              &ir_curve_template_entry_lookup::ir_curve_template_entry},
    };
}

/**
 * @brief The members of ir_curve_template_entries_filter, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ir_curve_template_entries_filter&) {
    return std::tuple{
        utility::codec::field{"ir_curve_config_id",
                              &ir_curve_template_entries_filter::ir_curve_config_id},
        utility::codec::field{"id_one_of", &ir_curve_template_entries_filter::id_one_of},
        utility::codec::field{"ir_curve_config_id_one_of",
                              &ir_curve_template_entries_filter::ir_curve_config_id_one_of},
    };
}

/**
 * @brief The members of ir_curve_template_entry_event, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ir_curve_template_entry_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &ir_curve_template_entry_event::event_id},
        utility::codec::field{"key", &ir_curve_template_entry_event::key},
        utility::codec::field{"action", &ir_curve_template_entry_event::action},
        utility::codec::field{"version", &ir_curve_template_entry_event::version},
        utility::codec::field{"occurred_at", &ir_curve_template_entry_event::occurred_at},
        utility::codec::field{"correlation_id", &ir_curve_template_entry_event::correlation_id},
    };
}

/**
 * @brief The members of ir_curve_template_entry_version_key, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ir_curve_template_entry_version_key&) {
    return std::tuple{
        utility::codec::field{"ir_curve_template_entry",
                              &ir_curve_template_entry_version_key::ir_curve_template_entry},
        utility::codec::field{"version", &ir_curve_template_entry_version_key::version},
    };
}

/**
 * @brief The members of ir_curve_template_entry_versions_filter, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const ir_curve_template_entry_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &ir_curve_template_entry_versions_filter::version},
        utility::codec::field{"from_version",
                              &ir_curve_template_entry_versions_filter::from_version},
        utility::codec::field{"to_version", &ir_curve_template_entry_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_ir_curve_template_entries_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_ir_curve_template_entries_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_ir_curve_template_entries_request::offset},
        utility::codec::field{"limit", &list_ir_curve_template_entries_request::limit},
        utility::codec::field{"order", &list_ir_curve_template_entries_request::order},
        utility::codec::field{"filter", &list_ir_curve_template_entries_request::filter},
        utility::codec::field{"as_of", &list_ir_curve_template_entries_request::as_of},
    };
}

/**
 * @brief The members of list_ir_curve_template_entries_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_ir_curve_template_entries_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_ir_curve_template_entries_response::result},
        utility::codec::field{"ir_curve_template_entries",
                              &list_ir_curve_template_entries_response::ir_curve_template_entries},
        utility::codec::field{"total", &list_ir_curve_template_entries_response::total},
    };
}

/**
 * @brief The members of get_ir_curve_template_entry_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_ir_curve_template_entry_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_ir_curve_template_entry_request::key},
    };
}

/**
 * @brief The members of get_ir_curve_template_entry_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_ir_curve_template_entry_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_ir_curve_template_entry_response::result},
        utility::codec::field{"ir_curve_template_entry",
                              &get_ir_curve_template_entry_response::ir_curve_template_entry},
    };
}

/**
 * @brief The members of get_many_ir_curve_template_entries_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_ir_curve_template_entries_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_ir_curve_template_entries_request::keys},
    };
}

/**
 * @brief The members of get_many_ir_curve_template_entries_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_ir_curve_template_entries_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_ir_curve_template_entries_response::result},
        utility::codec::field{"entries", &get_many_ir_curve_template_entries_response::entries},
    };
}

/**
 * @brief The members of put_ir_curve_template_entry_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_ir_curve_template_entry_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_ir_curve_template_entry_request::change},
        utility::codec::field{"intent", &put_ir_curve_template_entry_request::intent},
    };
}

/**
 * @brief The members of put_ir_curve_template_entry_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_ir_curve_template_entry_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_ir_curve_template_entry_response::result},
        utility::codec::field{"ir_curve_template_entry",
                              &put_ir_curve_template_entry_response::ir_curve_template_entry},
    };
}

/**
 * @brief The members of put_many_ir_curve_template_entries_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_ir_curve_template_entries_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_ir_curve_template_entries_request::changes},
        utility::codec::field{"intent", &put_many_ir_curve_template_entries_request::intent},
    };
}

/**
 * @brief The members of put_many_ir_curve_template_entries_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_ir_curve_template_entries_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_ir_curve_template_entries_response::result},
        utility::codec::field{
            "ir_curve_template_entries",
            &put_many_ir_curve_template_entries_response::ir_curve_template_entries},
    };
}

/**
 * @brief The members of delete_ir_curve_template_entry_request, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_ir_curve_template_entry_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_ir_curve_template_entry_request::removal},
        utility::codec::field{"intent", &delete_ir_curve_template_entry_request::intent},
    };
}

/**
 * @brief The members of delete_ir_curve_template_entry_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_ir_curve_template_entry_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_ir_curve_template_entry_response::result},
    };
}

/**
 * @brief The members of delete_many_ir_curve_template_entries_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_ir_curve_template_entries_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_ir_curve_template_entries_request::removals},
        utility::codec::field{"intent", &delete_many_ir_curve_template_entries_request::intent},
    };
}

/**
 * @brief The members of delete_many_ir_curve_template_entries_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_ir_curve_template_entries_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_ir_curve_template_entries_response::result},
    };
}

/**
 * @brief The members of list_ir_curve_template_entry_versions_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_ir_curve_template_entry_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_ir_curve_template_entry_versions_request::key},
        utility::codec::field{"offset", &list_ir_curve_template_entry_versions_request::offset},
        utility::codec::field{"limit", &list_ir_curve_template_entry_versions_request::limit},
        utility::codec::field{"order", &list_ir_curve_template_entry_versions_request::order},
        utility::codec::field{"filter", &list_ir_curve_template_entry_versions_request::filter},
    };
}

/**
 * @brief The members of list_ir_curve_template_entry_versions_response, named as the wire names
 * them and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_ir_curve_template_entry_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_ir_curve_template_entry_versions_response::result},
        utility::codec::field{"versions",
                              &list_ir_curve_template_entry_versions_response::versions},
        utility::codec::field{"total", &list_ir_curve_template_entry_versions_response::total},
    };
}

/**
 * @brief The members of get_ir_curve_template_entry_version_request, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_ir_curve_template_entry_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_ir_curve_template_entry_version_request::key},
    };
}

/**
 * @brief The members of get_ir_curve_template_entry_version_response, named as the wire names them
 * and in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_ir_curve_template_entry_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_ir_curve_template_entry_version_response::result},
        utility::codec::field{"version", &get_ir_curve_template_entry_version_response::version},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::ir_curve_template_entry_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::synthetic::messaging::ir_curve_template_entry_key& v);
    static ores::synthetic::messaging::ir_curve_template_entry_key read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::ir_curve_template_entry_key& v);
    static ores::synthetic::messaging::ir_curve_template_entry_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::ir_curve_template_entry_write> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::ir_curve_template_entry_write& v);
    static ores::synthetic::messaging::ir_curve_template_entry_write
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::ir_curve_template_entry_write& v);
    static ores::synthetic::messaging::ir_curve_template_entry_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::ir_curve_template_entry_change> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::ir_curve_template_entry_change& v);
    static ores::synthetic::messaging::ir_curve_template_entry_change
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::ir_curve_template_entry_change& v);
    static ores::synthetic::messaging::ir_curve_template_entry_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::ir_curve_template_entry_removal> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::ir_curve_template_entry_removal& v);
    static ores::synthetic::messaging::ir_curve_template_entry_removal
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::ir_curve_template_entry_removal& v);
    static ores::synthetic::messaging::ir_curve_template_entry_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::ir_curve_template_entry_lookup> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::ir_curve_template_entry_lookup& v);
    static ores::synthetic::messaging::ir_curve_template_entry_lookup
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::ir_curve_template_entry_lookup& v);
    static ores::synthetic::messaging::ir_curve_template_entry_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::ir_curve_template_entries_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::ir_curve_template_entries_filter& v);
    static ores::synthetic::messaging::ir_curve_template_entries_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::ir_curve_template_entries_filter& v);
    static ores::synthetic::messaging::ir_curve_template_entries_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::ir_curve_template_entry_event> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::ir_curve_template_entry_event& v);
    static ores::synthetic::messaging::ir_curve_template_entry_event
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::ir_curve_template_entry_event& v);
    static ores::synthetic::messaging::ir_curve_template_entry_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::ir_curve_template_entry_version_key> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::ir_curve_template_entry_version_key& v);
    static ores::synthetic::messaging::ir_curve_template_entry_version_key
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::ir_curve_template_entry_version_key& v);
    static ores::synthetic::messaging::ir_curve_template_entry_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::ir_curve_template_entry_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::ir_curve_template_entry_versions_filter& v);
    static ores::synthetic::messaging::ir_curve_template_entry_versions_filter
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::ir_curve_template_entry_versions_filter& v);
    static ores::synthetic::messaging::ir_curve_template_entry_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_ir_curve_template_entries_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_ir_curve_template_entries_request& v);
    static ores::synthetic::messaging::list_ir_curve_template_entries_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_ir_curve_template_entries_request& v);
    static ores::synthetic::messaging::list_ir_curve_template_entries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_ir_curve_template_entries_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_ir_curve_template_entries_response& v);
    static ores::synthetic::messaging::list_ir_curve_template_entries_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::list_ir_curve_template_entries_response& v);
    static ores::synthetic::messaging::list_ir_curve_template_entries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_ir_curve_template_entry_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_ir_curve_template_entry_request& v);
    static ores::synthetic::messaging::get_ir_curve_template_entry_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_ir_curve_template_entry_request& v);
    static ores::synthetic::messaging::get_ir_curve_template_entry_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_ir_curve_template_entry_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_ir_curve_template_entry_response& v);
    static ores::synthetic::messaging::get_ir_curve_template_entry_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_ir_curve_template_entry_response& v);
    static ores::synthetic::messaging::get_ir_curve_template_entry_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_ir_curve_template_entries_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_ir_curve_template_entries_request& v);
    static ores::synthetic::messaging::get_many_ir_curve_template_entries_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_ir_curve_template_entries_request& v);
    static ores::synthetic::messaging::get_many_ir_curve_template_entries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_many_ir_curve_template_entries_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_many_ir_curve_template_entries_response& v);
    static ores::synthetic::messaging::get_many_ir_curve_template_entries_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_many_ir_curve_template_entries_response& v);
    static ores::synthetic::messaging::get_many_ir_curve_template_entries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_ir_curve_template_entry_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_ir_curve_template_entry_request& v);
    static ores::synthetic::messaging::put_ir_curve_template_entry_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_ir_curve_template_entry_request& v);
    static ores::synthetic::messaging::put_ir_curve_template_entry_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_ir_curve_template_entry_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_ir_curve_template_entry_response& v);
    static ores::synthetic::messaging::put_ir_curve_template_entry_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_ir_curve_template_entry_response& v);
    static ores::synthetic::messaging::put_ir_curve_template_entry_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_ir_curve_template_entries_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_ir_curve_template_entries_request& v);
    static ores::synthetic::messaging::put_many_ir_curve_template_entries_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_ir_curve_template_entries_request& v);
    static ores::synthetic::messaging::put_many_ir_curve_template_entries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::put_many_ir_curve_template_entries_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::put_many_ir_curve_template_entries_response& v);
    static ores::synthetic::messaging::put_many_ir_curve_template_entries_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::put_many_ir_curve_template_entries_response& v);
    static ores::synthetic::messaging::put_many_ir_curve_template_entries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_ir_curve_template_entry_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_ir_curve_template_entry_request& v);
    static ores::synthetic::messaging::delete_ir_curve_template_entry_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_ir_curve_template_entry_request& v);
    static ores::synthetic::messaging::delete_ir_curve_template_entry_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_ir_curve_template_entry_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_ir_curve_template_entry_response& v);
    static ores::synthetic::messaging::delete_ir_curve_template_entry_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::delete_ir_curve_template_entry_response& v);
    static ores::synthetic::messaging::delete_ir_curve_template_entry_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_ir_curve_template_entries_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_ir_curve_template_entries_request& v);
    static ores::synthetic::messaging::delete_many_ir_curve_template_entries_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_many_ir_curve_template_entries_request& v);
    static ores::synthetic::messaging::delete_many_ir_curve_template_entries_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::delete_many_ir_curve_template_entries_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::delete_many_ir_curve_template_entries_response& v);
    static ores::synthetic::messaging::delete_many_ir_curve_template_entries_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::delete_many_ir_curve_template_entries_response& v);
    static ores::synthetic::messaging::delete_many_ir_curve_template_entries_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_ir_curve_template_entry_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_ir_curve_template_entry_versions_request& v);
    static ores::synthetic::messaging::list_ir_curve_template_entry_versions_request
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_ir_curve_template_entry_versions_request& v);
    static ores::synthetic::messaging::list_ir_curve_template_entry_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::list_ir_curve_template_entry_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::list_ir_curve_template_entry_versions_response& v);
    static ores::synthetic::messaging::list_ir_curve_template_entry_versions_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::list_ir_curve_template_entry_versions_response& v);
    static ores::synthetic::messaging::list_ir_curve_template_entry_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_ir_curve_template_entry_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_ir_curve_template_entry_version_request& v);
    static ores::synthetic::messaging::get_ir_curve_template_entry_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::synthetic::messaging::get_ir_curve_template_entry_version_request& v);
    static ores::synthetic::messaging::get_ir_curve_template_entry_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_SYNTHETIC_API_EXPORT
    wire_traits<ores::synthetic::messaging::get_ir_curve_template_entry_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::synthetic::messaging::get_ir_curve_template_entry_version_response& v);
    static ores::synthetic::messaging::get_ir_curve_template_entry_version_response
    read_json(std::string_view text);
    static std::string write_msgpack(
        const ores::synthetic::messaging::get_ir_curve_template_entry_version_response& v);
    static ores::synthetic::messaging::get_ir_curve_template_entry_version_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
