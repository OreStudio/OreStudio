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
#ifndef ORES_IAM_API_MESSAGING_SEED_PROFILE_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_SEED_PROFILE_PROTOCOL_HPP

#include "ores.iam.api/domain/seed_profile.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct seed_profile_key {
    boost::uuids::uuid id;
};

struct seed_profile_write {
    boost::uuids::uuid id;
    std::string code;
    std::string name;
    std::string summary;
    std::string audience;
    std::string bullets_json;
    std::string tenant_type;
    std::string tenant_name;
    std::string tenant_code;
    std::string tenant_hostname;
    std::string admin_username;
    std::string admin_email;
    bool inherits_admin_password;
    bool force_password_change;
    int display_order;
};

struct seed_profile_change {
    seed_profile_write write;
    ores::utility::domain::precondition precondition;
};

struct seed_profile_removal {
    seed_profile_key key;
    ores::utility::domain::precondition precondition = ores::utility::domain::removal_precondition;
};

struct seed_profile_lookup {
    seed_profile_key key;
    std::optional<ores::iam::domain::seed_profile> seed_profile;
};

struct seed_profiles_filter {
    std::optional<std::vector<boost::uuids::uuid>> id_one_of;
};

struct seed_profile_event {
    boost::uuids::uuid event_id;
    seed_profile_key key;
    std::string action;
    std::uint32_t version;
    std::chrono::system_clock::time_point occurred_at;
    std::optional<std::string> correlation_id;
};

struct seed_profile_version_key {
    seed_profile_key seed_profile;
    std::uint32_t version;
};

struct seed_profile_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_seed_profiles_request {
    using response_type = struct list_seed_profiles_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profiles.list";
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
    std::optional<seed_profiles_filter> filter;
};

struct list_seed_profiles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::seed_profile> seed_profiles;
    std::uint64_t total;
};

struct get_seed_profile_request {
    using response_type = struct get_seed_profile_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profiles.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    seed_profile_key key;
};

struct get_seed_profile_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::seed_profile> seed_profile;
};

struct get_many_seed_profiles_request {
    using response_type = struct get_many_seed_profiles_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profiles.get_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<seed_profile_key> keys;
};

struct get_many_seed_profiles_response {
    ores::utility::domain::result result;
    std::vector<seed_profile_lookup> entries;
};

struct put_seed_profile_request {
    using response_type = struct put_seed_profile_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profiles.put";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    seed_profile_change change;
    ores::utility::domain::change_intent intent;
};

struct put_seed_profile_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::seed_profile> seed_profile;
};

struct put_many_seed_profiles_request {
    using response_type = struct put_many_seed_profiles_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profiles.put_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<seed_profile_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_seed_profiles_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::seed_profile> seed_profiles;
};

struct delete_seed_profile_request {
    using response_type = struct delete_seed_profile_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profiles.delete";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    seed_profile_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_seed_profile_response {
    ores::utility::domain::result result;
};

struct delete_many_seed_profiles_request {
    using response_type = struct delete_many_seed_profiles_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profiles.delete_many";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    std::vector<seed_profile_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_seed_profiles_response {
    ores::utility::domain::result result;
};

struct list_seed_profile_versions_request {
    using response_type = struct list_seed_profile_versions_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profiles_versions.list";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    seed_profile_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<seed_profile_versions_filter> filter;
};

struct list_seed_profile_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::seed_profile> versions;
    std::uint64_t total;
};

struct get_seed_profile_version_request {
    using response_type = struct get_seed_profile_version_response;
    static constexpr std::string_view nats_subject = "iam.v1.seed_profiles_versions.get";
    /**
     * @brief Whether the caller must have established a session first.
     *
     * An operation that produces the session cannot present one, so a client
     * reads this rather than assuming every call carries a token.
     */
    static constexpr bool requires_session = true;
    seed_profile_version_key key;
};

struct get_seed_profile_version_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::seed_profile> version;
};

/**
 * @brief The subjects this resource's changes are announced on.
 *
 * An event reports what happened and no caller asked for it, so its last
 * segment is the action rather than a verb. One payload is therefore addressed
 * by three subjects, and a subscriber that wants one action subscribes to one
 * of them.
 */
namespace seed_profile_event_subjects {
inline constexpr std::string_view created = "iam.v1.seed_profiles_events.created";
inline constexpr std::string_view updated = "iam.v1.seed_profiles_events.updated";
inline constexpr std::string_view deleted = "iam.v1.seed_profiles_events.deleted";
}

}

#endif
