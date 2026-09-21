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
#ifndef ORES_IAM_API_MESSAGING_ACCOUNT_CONTACT_INFORMATION_PROTOCOL_HPP
#define ORES_IAM_API_MESSAGING_ACCOUNT_CONTACT_INFORMATION_PROTOCOL_HPP

#include "ores.iam.api/domain/account_contact_information.hpp"
#include "ores.utility/domain/protocol.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::messaging {

struct account_contact_information_key {
    boost::uuids::uuid id;
};

struct account_contact_information_write {
    boost::uuids::uuid id;
    boost::uuids::uuid account_id;
    std::string street_line_1;
    std::string street_line_2;
    std::string city;
    std::string state;
    std::string country_code;
    std::string postal_code;
    std::string phone;
    std::string email;
    std::string web_page;
};

struct account_contact_information_change {
    account_contact_information_write write;
    ores::utility::domain::precondition precondition;
};

struct account_contact_information_removal {
    account_contact_information_key key;
    ores::utility::domain::precondition precondition;
};

struct account_contact_information_lookup {
    account_contact_information_key key;
    std::optional<ores::iam::domain::account_contact_information> account_contact_information;
};

struct account_contact_informations_filter {
    std::optional<boost::uuids::uuid> account_id;
};

struct account_contact_information_version_key {
    account_contact_information_key account_contact_information;
    std::uint32_t version;
};

struct account_contact_information_versions_filter {
    std::optional<std::uint32_t> version;
    std::optional<std::uint32_t> from_version;
    std::optional<std::uint32_t> to_version;
};

struct list_account_contact_informations_request {
    using response_type = struct list_account_contact_informations_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.list";
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_contact_informations_filter> filter;
};

struct list_account_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_contact_information> account_contact_informations;
    std::uint64_t total;
};

struct get_account_contact_information_request {
    using response_type = struct get_account_contact_information_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.get";
    account_contact_information_key key;
};

struct get_account_contact_information_response {
    ores::utility::domain::result result;
    std::optional<ores::iam::domain::account_contact_information> account_contact_information;
};

struct get_many_account_contact_informations_request {
    using response_type = struct get_many_account_contact_informations_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.get_many";
    std::vector<account_contact_information_key> keys;
};

struct get_many_account_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<account_contact_information_lookup> entries;
};

struct put_account_contact_information_request {
    using response_type = struct put_account_contact_information_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.put";
    account_contact_information_change change;
    ores::utility::domain::change_intent intent;
};

struct put_account_contact_information_response {
    ores::utility::domain::result result;
    ores::iam::domain::account_contact_information account_contact_information;
};

struct put_many_account_contact_informations_request {
    using response_type = struct put_many_account_contact_informations_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.put_many";
    std::vector<account_contact_information_change> changes;
    ores::utility::domain::change_intent intent;
};

struct put_many_account_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_contact_information> account_contact_informations;
};

struct delete_account_contact_information_request {
    using response_type = struct delete_account_contact_information_response;
    static constexpr std::string_view nats_subject = "iam.v1.account_contact_informations.delete";
    account_contact_information_removal removal;
    ores::utility::domain::change_intent intent;
};

struct delete_account_contact_information_response {
    ores::utility::domain::result result;
};

struct delete_many_account_contact_informations_request {
    using response_type = struct delete_many_account_contact_informations_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.account_contact_informations.delete_many";
    std::vector<account_contact_information_removal> removals;
    ores::utility::domain::change_intent intent;
};

struct delete_many_account_contact_informations_response {
    ores::utility::domain::result result;
};

struct list_by_account_id_account_contact_informations_request {
    using response_type = struct list_by_account_id_account_contact_informations_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.account_contact_informations.list_by_account_id";
    boost::uuids::uuid account_id;
    ores::utility::domain::scope scope = ores::utility::domain::scope::direct;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_contact_informations_filter> filter;
};

struct list_by_account_id_account_contact_informations_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_contact_information> account_contact_informations;
    std::uint64_t total;
};

struct list_account_contact_information_versions_request {
    using response_type = struct list_account_contact_information_versions_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.account_contact_informations_versions.list";
    account_contact_information_key key;
    std::uint32_t offset = 0;
    std::uint32_t limit = 100;
    ores::utility::domain::order order;
    std::optional<account_contact_information_versions_filter> filter;
};

struct list_account_contact_information_versions_response {
    ores::utility::domain::result result;
    std::vector<ores::iam::domain::account_contact_information> versions;
    std::uint64_t total;
};

struct get_account_contact_information_version_request {
    using response_type = struct get_account_contact_information_version_response;
    static constexpr std::string_view nats_subject =
        "iam.v1.account_contact_informations_versions.get";
    account_contact_information_version_key key;
};

struct get_account_contact_information_version_response {
    ores::utility::domain::result result;
    ores::iam::domain::account_contact_information version;
};

}

#endif
