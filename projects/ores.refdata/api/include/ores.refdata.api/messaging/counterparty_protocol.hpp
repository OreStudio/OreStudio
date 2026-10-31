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
#ifndef ORES_REFDATA_API_MESSAGING_COUNTERPARTY_PROTOCOL_HPP
#define ORES_REFDATA_API_MESSAGING_COUNTERPARTY_PROTOCOL_HPP

#include "ores.refdata.api/domain/counterparty.hpp"
#include "ores.refdata.api/domain/counterparty_contact_information.hpp"
#include "ores.refdata.api/domain/counterparty_identifier.hpp"
#include "ores.refdata.api/domain/csa.hpp"
#include "ores.refdata.api/domain/csa_eligible_currency.hpp"
#include "ores.refdata.api/domain/netting_agreement.hpp"
#include "ores.refdata.api/domain/netting_set.hpp"
#include "ores.refdata.api/domain/netting_set_identifier.hpp"
#include "ores.refdata.api/export.hpp"
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

namespace ores::refdata::messaging {

struct counterparty_key {
    std::string short_code;
};

struct counterparty_write {
    boost::uuids::uuid id;
    std::string short_code;
    std::string full_name;
    std::optional<std::string> transliterated_name;
    std::string party_type;
    std::optional<boost::uuids::uuid> parent_counterparty_id;
    std::string status;
    std::optional<boost::uuids::uuid> image_id;
};

struct counterparty_change {
    counterparty_write write;
    ores::utility::domain::precondition precondition;
};

struct counterparty_removal {
    counterparty_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct counterparty_lookup {
    counterparty_key key;
    std::optional<ores::refdata::domain::counterparty> counterparty;
};

struct counterparties_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct counterparty_event {
    boost::uuids::uuid event_id;
    counterparty_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct counterparty_version_key {
    counterparty_key counterparty;
    std::uint32_t version;
};

struct counterparty_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_counterparties_request {
    using response_type = struct list_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.list";
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
    std::optional<counterparties_filter> filter;
    std::optional<std::string> as_of;
};

struct list_counterparties_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty> counterparties;
    std::uint64_t total;
};

struct get_counterparty_request {
    using response_type = struct get_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_key key;
};

struct get_counterparty_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty> counterparty;
};

struct get_many_counterparties_request {
    using response_type = struct get_many_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_key> keys;
};

struct get_many_counterparties_response {
    ores::utility::domain::result result;
    std::vector<counterparty_lookup> entries;
};

struct put_counterparty_request {
    using response_type = struct put_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_change change;
    ores::utility::domain::change_intent intent;
};

struct put_counterparty_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty> counterparty;
};

struct put_many_counterparties_request {
    using response_type = struct put_many_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_counterparties_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty> counterparties;
};

struct delete_counterparty_request {
    using response_type = struct delete_counterparty_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_counterparty_response {
    ores::utility::domain::result result;
};

struct delete_many_counterparties_request {
    using response_type = struct delete_many_counterparties_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<counterparty_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_counterparties_response {
    ores::utility::domain::result result;
};

struct list_counterparty_versions_request {
    using response_type = struct list_counterparty_versions_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<counterparty_versions_filter> filter;
};

struct list_counterparty_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::refdata::domain::counterparty> versions;
    std::uint64_t total;
};

struct get_counterparty_version_request {
    using response_type = struct get_counterparty_version_response;
    static constexpr std::string_view nats_subject = "refdata.v1.counterparties_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    counterparty_version_key key;
};

struct get_counterparty_version_response {
    ores::utility::domain::result result;
    std::optional<ores::refdata::domain::counterparty> version;
};

/**
 * @brief Reads a counterparty as it stood at a specific version, together
 * with its identifiers and contact information as they stood during that
 * same version's [valid_from, valid_to) window. See the "Temporal composite
 * entity versioning" architecture doc.
 */
struct get_counterparty_composite_as_of_request {
    using response_type = struct get_counterparty_composite_as_of_response;
    static constexpr std::string_view nats_subject =
        "refdata.v1.ops.get_counterparty_composite_as_of";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    boost::uuids::uuid id;
    int version = 0;
};

struct get_counterparty_composite_as_of_response {
    bool success = false;
    std::string message;
    ores::refdata::domain::counterparty counterparty;
    std::vector<ores::refdata::domain::counterparty_identifier> identifiers;
    std::vector<ores::refdata::domain::counterparty_contact_information> contacts;
};

/**
 * @brief Writes a counterparty and the children a confirmed review staged, in
 * one transaction. The eight writes the confirm lists are one act: a refusal
 * or a failure leaves none of them behind.
 */
struct put_counterparty_composite_request {
    using response_type = struct put_counterparty_composite_response;
    static constexpr std::string_view nats_subject = "refdata.v1.ops.put_counterparty_composite";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    ores::utility::domain::change_intent intent;
    ores::refdata::domain::counterparty counterparty;
    std::vector<ores::refdata::domain::counterparty_identifier> identifiers;
    std::vector<ores::refdata::domain::counterparty_contact_information> contacts;
    std::vector<ores::refdata::domain::netting_agreement> agreements;
    std::vector<ores::refdata::domain::netting_set> netting_sets;
    std::vector<ores::refdata::domain::netting_set_identifier> netting_set_identifiers;
    std::vector<ores::refdata::domain::csa> csas;
    std::vector<ores::refdata::domain::csa_eligible_currency> eligible_currencies;
};

struct put_counterparty_composite_response {
    ores::utility::domain::result result;
    ores::refdata::domain::counterparty counterparty;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace counterparty_event_subjects {
inline constexpr std::string_view created = "refdata.v1.counterparties_events.created";
inline constexpr std::string_view updated = "refdata.v1.counterparties_events.updated";
inline constexpr std::string_view deleted = "refdata.v1.counterparties_events.deleted";
}
/**
 * @brief The members of counterparty_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_key&) {
    return std::tuple{
        utility::codec::field{"short_code", &counterparty_key::short_code},
    };
}

/**
 * @brief The members of counterparty_write, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_write&) {
    return std::tuple{
        utility::codec::field{"id", &counterparty_write::id},
        utility::codec::field{"short_code", &counterparty_write::short_code},
        utility::codec::field{"full_name", &counterparty_write::full_name},
        utility::codec::field{"transliterated_name", &counterparty_write::transliterated_name},
        utility::codec::field{"party_type", &counterparty_write::party_type},
        utility::codec::field{"parent_counterparty_id",
                              &counterparty_write::parent_counterparty_id},
        utility::codec::field{"status", &counterparty_write::status},
        utility::codec::field{"image_id", &counterparty_write::image_id},
    };
}

/**
 * @brief The members of counterparty_change, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_change&) {
    return std::tuple{
        utility::codec::field{"write", &counterparty_change::write},
        utility::codec::field{"precondition", &counterparty_change::precondition},
    };
}

/**
 * @brief The members of counterparty_removal, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_removal&) {
    return std::tuple{
        utility::codec::field{"key", &counterparty_removal::key},
        utility::codec::field{"precondition", &counterparty_removal::precondition},
    };
}

/**
 * @brief The members of counterparty_lookup, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_lookup&) {
    return std::tuple{
        utility::codec::field{"key", &counterparty_lookup::key},
        utility::codec::field{"counterparty", &counterparty_lookup::counterparty},
    };
}

/**
 * @brief The members of counterparties_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparties_filter&) {
    return std::tuple{
        utility::codec::field{"id_one_of", &counterparties_filter::id_one_of},
    };
}

/**
 * @brief The members of counterparty_event, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_event&) {
    return std::tuple{
        utility::codec::field{"event_id", &counterparty_event::event_id},
        utility::codec::field{"key", &counterparty_event::key},
        utility::codec::field{"action", &counterparty_event::action},
        utility::codec::field{"version", &counterparty_event::version},
        utility::codec::field{"occurred_at", &counterparty_event::occurred_at},
        utility::codec::field{"correlation_id", &counterparty_event::correlation_id},
    };
}

/**
 * @brief The members of counterparty_version_key, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_version_key&) {
    return std::tuple{
        utility::codec::field{"counterparty", &counterparty_version_key::counterparty},
        utility::codec::field{"version", &counterparty_version_key::version},
    };
}

/**
 * @brief The members of counterparty_versions_filter, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const counterparty_versions_filter&) {
    return std::tuple{
        utility::codec::field{"version", &counterparty_versions_filter::version},
        utility::codec::field{"from_version", &counterparty_versions_filter::from_version},
        utility::codec::field{"to_version", &counterparty_versions_filter::to_version},
    };
}

/**
 * @brief The members of list_counterparties_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_counterparties_request&) {
    return std::tuple{
        utility::codec::field{"offset", &list_counterparties_request::offset},
        utility::codec::field{"limit", &list_counterparties_request::limit},
        utility::codec::field{"order", &list_counterparties_request::order},
        utility::codec::field{"filter", &list_counterparties_request::filter},
        utility::codec::field{"as_of", &list_counterparties_request::as_of},
    };
}

/**
 * @brief The members of list_counterparties_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_counterparties_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_counterparties_response::result},
        utility::codec::field{"counterparties", &list_counterparties_response::counterparties},
        utility::codec::field{"total", &list_counterparties_response::total},
    };
}

/**
 * @brief The members of get_counterparty_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_counterparty_request::key},
    };
}

/**
 * @brief The members of get_counterparty_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_counterparty_response::result},
        utility::codec::field{"counterparty", &get_counterparty_response::counterparty},
    };
}

/**
 * @brief The members of get_many_counterparties_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_counterparties_request&) {
    return std::tuple{
        utility::codec::field{"keys", &get_many_counterparties_request::keys},
    };
}

/**
 * @brief The members of get_many_counterparties_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_many_counterparties_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_many_counterparties_response::result},
        utility::codec::field{"entries", &get_many_counterparties_response::entries},
    };
}

/**
 * @brief The members of put_counterparty_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_counterparty_request&) {
    return std::tuple{
        utility::codec::field{"change", &put_counterparty_request::change},
        utility::codec::field{"intent", &put_counterparty_request::intent},
    };
}

/**
 * @brief The members of put_counterparty_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_counterparty_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_counterparty_response::result},
        utility::codec::field{"counterparty", &put_counterparty_response::counterparty},
    };
}

/**
 * @brief The members of put_many_counterparties_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_counterparties_request&) {
    return std::tuple{
        utility::codec::field{"changes", &put_many_counterparties_request::changes},
        utility::codec::field{"intent", &put_many_counterparties_request::intent},
    };
}

/**
 * @brief The members of put_many_counterparties_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_many_counterparties_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_many_counterparties_response::result},
        utility::codec::field{"counterparties", &put_many_counterparties_response::counterparties},
    };
}

/**
 * @brief The members of delete_counterparty_request, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_counterparty_request&) {
    return std::tuple{
        utility::codec::field{"removal", &delete_counterparty_request::removal},
        utility::codec::field{"intent", &delete_counterparty_request::intent},
    };
}

/**
 * @brief The members of delete_counterparty_response, named as the wire names them and in the order
 * the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_counterparty_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_counterparty_response::result},
    };
}

/**
 * @brief The members of delete_many_counterparties_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_counterparties_request&) {
    return std::tuple{
        utility::codec::field{"removals", &delete_many_counterparties_request::removals},
        utility::codec::field{"intent", &delete_many_counterparties_request::intent},
    };
}

/**
 * @brief The members of delete_many_counterparties_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const delete_many_counterparties_response&) {
    return std::tuple{
        utility::codec::field{"result", &delete_many_counterparties_response::result},
    };
}

/**
 * @brief The members of list_counterparty_versions_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_counterparty_versions_request&) {
    return std::tuple{
        utility::codec::field{"key", &list_counterparty_versions_request::key},
        utility::codec::field{"offset", &list_counterparty_versions_request::offset},
        utility::codec::field{"limit", &list_counterparty_versions_request::limit},
        utility::codec::field{"order", &list_counterparty_versions_request::order},
        utility::codec::field{"filter", &list_counterparty_versions_request::filter},
    };
}

/**
 * @brief The members of list_counterparty_versions_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const list_counterparty_versions_response&) {
    return std::tuple{
        utility::codec::field{"result", &list_counterparty_versions_response::result},
        utility::codec::field{"versions", &list_counterparty_versions_response::versions},
        utility::codec::field{"total", &list_counterparty_versions_response::total},
    };
}

/**
 * @brief The members of get_counterparty_version_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_version_request&) {
    return std::tuple{
        utility::codec::field{"key", &get_counterparty_version_request::key},
    };
}

/**
 * @brief The members of get_counterparty_version_response, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_version_response&) {
    return std::tuple{
        utility::codec::field{"result", &get_counterparty_version_response::result},
        utility::codec::field{"version", &get_counterparty_version_response::version},
    };
}

/**
 * @brief The members of get_counterparty_composite_as_of_request, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_composite_as_of_request&) {
    return std::tuple{
        utility::codec::field{"id", &get_counterparty_composite_as_of_request::id},
        utility::codec::field{"version", &get_counterparty_composite_as_of_request::version},
    };
}

/**
 * @brief The members of get_counterparty_composite_as_of_response, named as the wire names them and
 * in the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const get_counterparty_composite_as_of_response&) {
    return std::tuple{
        utility::codec::field{"success", &get_counterparty_composite_as_of_response::success},
        utility::codec::field{"message", &get_counterparty_composite_as_of_response::message},
        utility::codec::field{"counterparty",
                              &get_counterparty_composite_as_of_response::counterparty},
        utility::codec::field{"identifiers",
                              &get_counterparty_composite_as_of_response::identifiers},
        utility::codec::field{"contacts", &get_counterparty_composite_as_of_response::contacts},
    };
}

/**
 * @brief The members of put_counterparty_composite_request, named as the wire names them and in the
 * order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_counterparty_composite_request&) {
    return std::tuple{
        utility::codec::field{"intent", &put_counterparty_composite_request::intent},
        utility::codec::field{"counterparty", &put_counterparty_composite_request::counterparty},
        utility::codec::field{"identifiers", &put_counterparty_composite_request::identifiers},
        utility::codec::field{"contacts", &put_counterparty_composite_request::contacts},
        utility::codec::field{"agreements", &put_counterparty_composite_request::agreements},
        utility::codec::field{"netting_sets", &put_counterparty_composite_request::netting_sets},
        utility::codec::field{"netting_set_identifiers",
                              &put_counterparty_composite_request::netting_set_identifiers},
        utility::codec::field{"csas", &put_counterparty_composite_request::csas},
        utility::codec::field{"eligible_currencies",
                              &put_counterparty_composite_request::eligible_currencies},
    };
}

/**
 * @brief The members of put_counterparty_composite_response, named as the wire names them and in
 * the order the type declares them.
 *
 * This is the one statement of the message's shape. The carriers find it by
 * argument-dependent lookup, so no caller passes a list, and a message that
 * holds another message or a domain type encodes it as its own object.
 */
[[nodiscard]] constexpr auto wire_fields(const put_counterparty_composite_response&) {
    return std::tuple{
        utility::codec::field{"result", &put_counterparty_composite_response::result},
        utility::codec::field{"counterparty", &put_counterparty_composite_response::counterparty},
    };
}


}
namespace ores::utility::codec {

template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_key& v);
    static ores::refdata::messaging::counterparty_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::counterparty_key& v);
    static ores::refdata::messaging::counterparty_key read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_write> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_write& v);
    static ores::refdata::messaging::counterparty_write read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::counterparty_write& v);
    static ores::refdata::messaging::counterparty_write
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_change> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_change& v);
    static ores::refdata::messaging::counterparty_change read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::counterparty_change& v);
    static ores::refdata::messaging::counterparty_change
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_removal> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_removal& v);
    static ores::refdata::messaging::counterparty_removal read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::counterparty_removal& v);
    static ores::refdata::messaging::counterparty_removal
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_lookup> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_lookup& v);
    static ores::refdata::messaging::counterparty_lookup read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::counterparty_lookup& v);
    static ores::refdata::messaging::counterparty_lookup
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparties_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparties_filter& v);
    static ores::refdata::messaging::counterparties_filter read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::counterparties_filter& v);
    static ores::refdata::messaging::counterparties_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_event> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_event& v);
    static ores::refdata::messaging::counterparty_event read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::counterparty_event& v);
    static ores::refdata::messaging::counterparty_event
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_version_key> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_version_key& v);
    static ores::refdata::messaging::counterparty_version_key read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::counterparty_version_key& v);
    static ores::refdata::messaging::counterparty_version_key
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::counterparty_versions_filter> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::counterparty_versions_filter& v);
    static ores::refdata::messaging::counterparty_versions_filter read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::counterparty_versions_filter& v);
    static ores::refdata::messaging::counterparty_versions_filter
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_counterparties_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_counterparties_request& v);
    static ores::refdata::messaging::list_counterparties_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_counterparties_request& v);
    static ores::refdata::messaging::list_counterparties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::list_counterparties_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::list_counterparties_response& v);
    static ores::refdata::messaging::list_counterparties_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_counterparties_response& v);
    static ores::refdata::messaging::list_counterparties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_counterparty_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_counterparty_request& v);
    static ores::refdata::messaging::get_counterparty_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_counterparty_request& v);
    static ores::refdata::messaging::get_counterparty_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::get_counterparty_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::get_counterparty_response& v);
    static ores::refdata::messaging::get_counterparty_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::get_counterparty_response& v);
    static ores::refdata::messaging::get_counterparty_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_counterparties_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_counterparties_request& v);
    static ores::refdata::messaging::get_many_counterparties_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_counterparties_request& v);
    static ores::refdata::messaging::get_many_counterparties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_many_counterparties_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_many_counterparties_response& v);
    static ores::refdata::messaging::get_many_counterparties_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_many_counterparties_response& v);
    static ores::refdata::messaging::get_many_counterparties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_counterparty_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_counterparty_request& v);
    static ores::refdata::messaging::put_counterparty_request read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_counterparty_request& v);
    static ores::refdata::messaging::put_counterparty_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::put_counterparty_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::put_counterparty_response& v);
    static ores::refdata::messaging::put_counterparty_response read_json(std::string_view text);
    static std::string write_msgpack(const ores::refdata::messaging::put_counterparty_response& v);
    static ores::refdata::messaging::put_counterparty_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_counterparties_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_counterparties_request& v);
    static ores::refdata::messaging::put_many_counterparties_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_counterparties_request& v);
    static ores::refdata::messaging::put_many_counterparties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_many_counterparties_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_many_counterparties_response& v);
    static ores::refdata::messaging::put_many_counterparties_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_many_counterparties_response& v);
    static ores::refdata::messaging::put_many_counterparties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_counterparty_request> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_counterparty_request& v);
    static ores::refdata::messaging::delete_counterparty_request read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_counterparty_request& v);
    static ores::refdata::messaging::delete_counterparty_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT wire_traits<ores::refdata::messaging::delete_counterparty_response> {
    static constexpr bool is_generated = true;

    static std::string write_json(const ores::refdata::messaging::delete_counterparty_response& v);
    static ores::refdata::messaging::delete_counterparty_response read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_counterparty_response& v);
    static ores::refdata::messaging::delete_counterparty_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_counterparties_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_counterparties_request& v);
    static ores::refdata::messaging::delete_many_counterparties_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_counterparties_request& v);
    static ores::refdata::messaging::delete_many_counterparties_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::delete_many_counterparties_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::delete_many_counterparties_response& v);
    static ores::refdata::messaging::delete_many_counterparties_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::delete_many_counterparties_response& v);
    static ores::refdata::messaging::delete_many_counterparties_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_versions_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_versions_request& v);
    static ores::refdata::messaging::list_counterparty_versions_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_counterparty_versions_request& v);
    static ores::refdata::messaging::list_counterparty_versions_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::list_counterparty_versions_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::list_counterparty_versions_response& v);
    static ores::refdata::messaging::list_counterparty_versions_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::list_counterparty_versions_response& v);
    static ores::refdata::messaging::list_counterparty_versions_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_version_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_version_request& v);
    static ores::refdata::messaging::get_counterparty_version_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_version_request& v);
    static ores::refdata::messaging::get_counterparty_version_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_version_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_version_response& v);
    static ores::refdata::messaging::get_counterparty_version_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_version_response& v);
    static ores::refdata::messaging::get_counterparty_version_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_composite_as_of_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_composite_as_of_request& v);
    static ores::refdata::messaging::get_counterparty_composite_as_of_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_composite_as_of_request& v);
    static ores::refdata::messaging::get_counterparty_composite_as_of_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::get_counterparty_composite_as_of_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::get_counterparty_composite_as_of_response& v);
    static ores::refdata::messaging::get_counterparty_composite_as_of_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::get_counterparty_composite_as_of_response& v);
    static ores::refdata::messaging::get_counterparty_composite_as_of_response
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_counterparty_composite_request> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_counterparty_composite_request& v);
    static ores::refdata::messaging::put_counterparty_composite_request
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_counterparty_composite_request& v);
    static ores::refdata::messaging::put_counterparty_composite_request
    read_msgpack(std::span<const std::byte> data);
};
template <>
struct ORES_REFDATA_API_EXPORT
    wire_traits<ores::refdata::messaging::put_counterparty_composite_response> {
    static constexpr bool is_generated = true;

    static std::string
    write_json(const ores::refdata::messaging::put_counterparty_composite_response& v);
    static ores::refdata::messaging::put_counterparty_composite_response
    read_json(std::string_view text);
    static std::string
    write_msgpack(const ores::refdata::messaging::put_counterparty_composite_response& v);
    static ores::refdata::messaging::put_counterparty_composite_response
    read_msgpack(std::span<const std::byte> data);
};
}


#endif
