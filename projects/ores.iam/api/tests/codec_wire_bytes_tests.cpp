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
#include "ores.iam.api/domain/account_codec.hpp"
#include "ores.iam.api/domain/account_contact_information_codec.hpp"
#include "ores.iam.api/domain/account_credential_codec.hpp"
#include "ores.iam.api/domain/account_party_codec.hpp"
#include "ores.iam.api/domain/account_role_codec.hpp"
#include "ores.iam.api/domain/account_status_codec.hpp"
#include "ores.iam.api/domain/account_type_codec.hpp"
#include "ores.iam.api/domain/auth_event_codec.hpp"
#include "ores.iam.api/domain/login_info_codec.hpp"
#include "ores.iam.api/domain/permission_codec.hpp"
#include "ores.iam.api/domain/role_codec.hpp"
#include "ores.iam.api/domain/role_grant_request_codec.hpp"
#include "ores.iam.api/domain/role_grant_request_role_codec.hpp"
#include "ores.iam.api/domain/role_permission_codec.hpp"
#include "ores.iam.api/domain/run_grant_codec.hpp"
#include "ores.iam.api/domain/run_token_issue_codec.hpp"
#include "ores.iam.api/domain/seed_profile_codec.hpp"
#include "ores.iam.api/domain/seed_profile_parameter_codec.hpp"
#include "ores.iam.api/domain/seed_profile_step_codec.hpp"
#include "ores.iam.api/domain/session_codec.hpp"
#include "ores.iam.api/domain/session_sample_codec.hpp"
#include "ores.iam.api/domain/tenant_codec.hpp"
#include "ores.iam.api/domain/tenant_status_codec.hpp"
#include "ores.iam.api/domain/tenant_type_codec.hpp"
#include "ores.iam.api/generators/account_contact_information_generator.hpp"
#include "ores.iam.api/generators/account_credential_generator.hpp"
#include "ores.iam.api/generators/account_generator.hpp"
#include "ores.iam.api/generators/account_party_generator.hpp"
#include "ores.iam.api/generators/account_role_generator.hpp"
#include "ores.iam.api/generators/account_status_generator.hpp"
#include "ores.iam.api/generators/account_type_generator.hpp"
#include "ores.iam.api/generators/auth_event_generator.hpp"
#include "ores.iam.api/generators/login_info_generator.hpp"
#include "ores.iam.api/generators/permission_generator.hpp"
#include "ores.iam.api/generators/role_generator.hpp"
#include "ores.iam.api/generators/role_grant_request_generator.hpp"
#include "ores.iam.api/generators/role_grant_request_role_generator.hpp"
#include "ores.iam.api/generators/role_permission_generator.hpp"
#include "ores.iam.api/generators/run_grant_generator.hpp"
#include "ores.iam.api/generators/run_token_issue_generator.hpp"
#include "ores.iam.api/generators/seed_profile_generator.hpp"
#include "ores.iam.api/generators/seed_profile_parameter_generator.hpp"
#include "ores.iam.api/generators/seed_profile_step_generator.hpp"
#include "ores.iam.api/generators/session_generator.hpp"
#include "ores.iam.api/generators/session_sample_generator.hpp"
#include "ores.iam.api/generators/tenant_generator.hpp"
#include "ores.iam.api/generators/tenant_status_generator.hpp"
#include "ores.iam.api/generators/tenant_type_generator.hpp"
#include "ores.iam.api/messaging/account_operations_protocol.hpp"
#include "ores.iam.api/messaging/auth_event_operations_protocol.hpp"
#include "ores.iam.api/messaging/authorization_protocol.hpp"
#include "ores.iam.api/messaging/bootstrap_protocol.hpp"
#include "ores.iam.api/messaging/geo_operations_protocol.hpp"
#include "ores.iam.api/messaging/login_protocol.hpp"
#include "ores.iam.api/messaging/password_policy_protocol.hpp"
#include "ores.iam.api/messaging/registration_policy_protocol.hpp"
#include "ores.iam.api/messaging/reset_protocol.hpp"
#include "ores.iam.api/messaging/role_request_operations_protocol.hpp"
#include "ores.iam.api/messaging/run_grant_operations_protocol.hpp"
#include "ores.iam.api/messaging/session_operations_protocol.hpp"
#include "ores.iam.api/messaging/session_samples_protocol.hpp"
#include "ores.iam.api/messaging/session_statistics_operations_protocol.hpp"
#include "ores.iam.api/messaging/signup_protocol.hpp"
#include "ores.iam.api/messaging/storage_capability_operations_protocol.hpp"
#include "ores.iam.api/messaging/tenant_provisioning_protocol.hpp"
#include "ores.iam.api/messaging/tenant_session_protocol.hpp"
#include "ores.utility/codec/wire_traits.hpp"
#include "ores.utility/generation/generation_context.hpp"
#include "ores.utility/rfl/reflectors.hpp" // IWYU pragma: keep.
#include <catch2/catch_test_macros.hpp>
#include <rfl/json.hpp>
#include <rfl/msgpack.hpp>
#include <span>
#include <string>

namespace {

const std::string codec_tags("[codec]");

template <typename T>
std::string their_msgpack(const T& v) {
    const auto bytes = rfl::msgpack::write(v);
    return std::string(bytes.begin(), bytes.end());
}

std::span<const std::byte> bytes_of(const std::string& s) {
    return {reinterpret_cast<const std::byte*>(s.data()), s.size()};
}

/**
 * @brief The generated codec writes the bytes reflect-cpp wrote, in both
 * formats, and reads them back to the same bytes.
 */
template <typename T>
void same_bytes_as_reflect_cpp(const T& v) {
    using codec = ores::utility::codec::wire_traits<T>;
    CHECK(codec::is_generated);

    const auto json = codec::write_json(v);
    CHECK(json == rfl::json::write(v));
    CHECK(codec::write_json(codec::read_json(json)) == json);

    const auto msgpack = codec::write_msgpack(v);
    CHECK(msgpack == their_msgpack(v));
    CHECK(codec::write_msgpack(codec::read_msgpack(bytes_of(msgpack))) == msgpack);
}

template <typename... T>
void every_default_has_the_same_bytes() {
    (same_bytes_as_reflect_cpp(T{}), ...);
}

}

TEST_CASE("every iam domain type has reflect-cpp's bytes", codec_tags) {
    ores::utility::generation::generation_context ctx(20261102);
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_account(ctx));
    same_bytes_as_reflect_cpp(
        ores::iam::generators::generate_synthetic_account_contact_information(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_account_party(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_account_role(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_account_status(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_account_type(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_auth_event(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_login_info(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_permission(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_role(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_role_grant_request(ctx));
    same_bytes_as_reflect_cpp(
        ores::iam::generators::generate_synthetic_role_grant_request_role(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_role_permission(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_run_grant(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_run_token_issue(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_seed_profile(ctx));
    same_bytes_as_reflect_cpp(
        ores::iam::generators::generate_synthetic_seed_profile_parameter(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_seed_profile_step(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_session(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_session_sample(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_tenant(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_tenant_status(ctx));
    same_bytes_as_reflect_cpp(ores::iam::generators::generate_synthetic_tenant_type(ctx));
}

TEST_CASE("every iam operation message's default has reflect-cpp's bytes", codec_tags) {
    every_default_has_the_same_bytes<
        ores::iam::messaging::publish_accounts_from_dq_request,
        ores::iam::messaging::save_account_request,
        ores::iam::messaging::update_account_request,
        ores::iam::messaging::update_account_response,
        ores::iam::messaging::save_account_response,
        ores::iam::messaging::delete_account_request,
        ores::iam::messaging::delete_account_response,
        ores::iam::messaging::account_operation_result,
        ores::iam::messaging::lock_account_request,
        ores::iam::messaging::lock_account_response,
        ores::iam::messaging::unlock_account_request,
        ores::iam::messaging::unlock_account_response,
        ores::iam::messaging::reset_password_request,
        ores::iam::messaging::reset_password_response,
        ores::iam::messaging::change_password_request,
        ores::iam::messaging::change_password_response,
        ores::iam::messaging::update_my_email_request,
        ores::iam::messaging::update_my_email_response,
        ores::iam::messaging::set_my_default_party_request,
        ores::iam::messaging::set_my_default_party_response,
        ores::iam::messaging::select_party_request,
        ores::iam::messaging::switch_party_request,
        ores::iam::messaging::select_party_response,
        ores::iam::messaging::change_password_request_typed,
        ores::iam::messaging::update_self_account_request,
        ores::iam::messaging::update_self_account_response,
        ores::iam::messaging::update_self_account_contact_information_request,
        ores::iam::messaging::update_self_account_contact_information_response,
        ores::iam::messaging::get_my_account_contact_information_request,
        ores::iam::messaging::get_my_account_contact_information_response,
        ores::iam::messaging::get_my_account_request,
        ores::iam::messaging::get_my_account_response,
        ores::iam::messaging::get_my_login_info_request,
        ores::iam::messaging::get_my_login_info_response,
        ores::iam::messaging::get_my_sessions_request,
        ores::iam::messaging::get_my_sessions_response,
        ores::iam::messaging::my_party,
        ores::iam::messaging::get_my_parties_request,
        ores::iam::messaging::get_my_parties_response,
        ores::iam::messaging::set_reporting_line_request,
        ores::iam::messaging::set_reporting_line_response,
        ores::iam::messaging::reporting_tree_party,
        ores::iam::messaging::reporting_tree_node,
        ores::iam::messaging::get_reporting_tree_request,
        ores::iam::messaging::get_reporting_tree_response,
        ores::iam::messaging::attach_account_pictures_request,
        ores::iam::messaging::attach_account_pictures_response,
        ores::iam::messaging::auth_event,
        ores::iam::messaging::list_auth_events_request,
        ores::iam::messaging::list_auth_events_response,
        ores::iam::messaging::assign_role_request,
        ores::iam::messaging::assign_role_response,
        ores::iam::messaging::assign_role_by_name_response,
        ores::iam::messaging::assign_role_by_name_request,
        ores::iam::messaging::revoke_role_request,
        ores::iam::messaging::revoke_role_response,
        ores::iam::messaging::revoke_role_by_name_response,
        ores::iam::messaging::revoke_role_by_name_request,
        ores::iam::messaging::get_account_roles_request,
        ores::iam::messaging::get_my_roles_request,
        ores::iam::messaging::permission_resource_row,
        ores::iam::messaging::permission_area_count,
        ores::iam::messaging::list_account_permissions_request,
        ores::iam::messaging::list_my_permissions_request,
        ores::iam::messaging::permission_page_response,
        ores::iam::messaging::list_role_permissions_page_request,
        ores::iam::messaging::role_page_row,
        ores::iam::messaging::list_roles_page_request,
        ores::iam::messaging::role_page_response,
        ores::iam::messaging::role_holder,
        ores::iam::messaging::list_role_holders_request,
        ores::iam::messaging::role_holders_response,
        ores::iam::messaging::account_role_access,
        ores::iam::messaging::get_account_roles_response,
        ores::iam::messaging::get_role_permissions_request,
        ores::iam::messaging::get_role_permissions_response,
        ores::iam::messaging::put_role_permissions_request,
        ores::iam::messaging::suggest_role_commands_request,
        ores::iam::messaging::suggest_role_commands_response,
        ores::iam::messaging::bootstrap_status_request,
        ores::iam::messaging::bootstrap_status_response,
        ores::iam::messaging::create_initial_admin_request,
        ores::iam::messaging::create_initial_admin_response,
        ores::iam::messaging::lookup_country_request,
        ores::iam::messaging::lookup_country_response,
        ores::iam::messaging::party_summary,
        ores::iam::messaging::database_info,
        ores::iam::messaging::login_request,
        ores::iam::messaging::login_response,
        ores::iam::messaging::logout_request,
        ores::iam::messaging::logout_response,
        ores::iam::messaging::public_key_request,
        ores::iam::messaging::refresh_request,
        ores::iam::messaging::refresh_response,
        ores::iam::messaging::service_login_request,
        ores::iam::messaging::service_login_response,
        ores::iam::messaging::get_password_policy_request,
        ores::iam::messaging::get_password_policy_response,
        ores::iam::messaging::registration_policy_request,
        ores::iam::messaging::registration_policy_response,
        ores::iam::messaging::reset_tenant_command,
        ores::iam::messaging::reset_tenant_result,
        ores::iam::messaging::reset_system_command,
        ores::iam::messaging::reset_system_result,
        ores::iam::messaging::ask_for_roles_request,
        ores::iam::messaging::ask_for_roles_response,
        ores::iam::messaging::get_request_roles_request,
        ores::iam::messaging::requested_role,
        ores::iam::messaging::get_request_roles_response,
        ores::iam::messaging::create_run_grant_request,
        ores::iam::messaging::create_run_grant_response,
        ores::iam::messaging::revoke_run_grant_request,
        ores::iam::messaging::revoke_run_grant_response,
        ores::iam::messaging::exchange_run_grant_request,
        ores::iam::messaging::exchange_run_grant_response,
        ores::iam::messaging::session_view,
        ores::iam::messaging::get_active_sessions_request,
        ores::iam::messaging::get_active_sessions_response,
        ores::iam::messaging::end_session_request,
        ores::iam::messaging::end_session_response,
        ores::iam::messaging::session_sample_dto,
        ores::iam::messaging::get_session_samples_request,
        ores::iam::messaging::get_session_samples_response,
        ores::iam::messaging::session_statistics_row,
        ores::iam::messaging::get_session_statistics_request,
        ores::iam::messaging::get_session_statistics_response,
        ores::iam::messaging::signup_request,
        ores::iam::messaging::signup_response,
        ores::iam::messaging::mint_storage_capability_request,
        ores::iam::messaging::mint_storage_capability_response,
        ores::iam::messaging::provision_tenant_command,
        ores::iam::messaging::provision_tenant_command_response,
        ores::iam::messaging::provision_party_command,
        ores::iam::messaging::provision_party_command_response,
        ores::iam::messaging::enter_tenant_request,
        ores::iam::messaging::enter_tenant_response,
        ores::iam::messaging::leave_tenant_request,
        ores::iam::messaging::leave_tenant_response>();
}

TEST_CASE("an account credential's secrets stay off the wire", codec_tags) {
    ores::utility::generation::generation_context ctx(20261103);
    const auto credential = ores::iam::generators::generate_synthetic_account_credential(ctx);
    using codec = ores::utility::codec::wire_traits<ores::iam::domain::account_credential>;
    CHECK(codec::is_generated);
    for (const auto& secret :
         {credential.password_hash, credential.service_password_hash, credential.totp_secret}) {
        REQUIRE_FALSE(secret.empty());
        CHECK(codec::write_json(credential).find(secret) == std::string::npos);
        CHECK(codec::write_msgpack(credential).find(secret) == std::string::npos);
    }
    const auto json = codec::write_json(credential);
    CHECK(json.find("password_hash") == std::string::npos);
    CHECK(json.find("totp_secret") == std::string::npos);
    const auto read = codec::read_json(json);
    CHECK(read.password_hash.empty());
    CHECK(read.totp_secret.empty());
}
