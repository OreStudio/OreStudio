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
 * Template: cpp_domain_type_repository.cpp.mustache
 * To modify, update the template and regenerate.
 */
#include "ores.iam.core/repository/account_repository.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include "ores.iam.api/domain/account_json_io.hpp" // IWYU pragma: keep.
#include "ores.iam.core/repository/account_entity.hpp"
#include "ores.iam.core/repository/account_mapper.hpp"
#include <boost/lexical_cast.hpp>
#include <boost/uuid/uuid_io.hpp>
#include <array>
#include <format>
#include <openssl/evp.h>
#include <sqlgen/postgres.hpp>

namespace ores::iam::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string account_repository::sql() {
    return generate_create_table_sql<account_entity>(lg());
}

void account_repository::write(context ctx, const domain::account& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing account. " << "id: " << v.id;
    execute_write_query(ctx, account_mapper::map(v), lg(), "Writing account to database.");
}

void account_repository::write(context ctx, const std::vector<domain::account>& v) {
    BOOST_LOG_SEV(lg(), debug) << "Writing accounts. Count: " << v.size();
    execute_write_query(ctx, account_mapper::map(v), lg(), "Writing accounts to database.");
}

std::vector<domain::account> account_repository::read_latest(context ctx) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c);

    return execute_read_query<account_entity, domain::account>(
        ctx,
        query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(),
        "Reading latest accounts");
}

std::vector<domain::account> account_repository::read_latest(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    return execute_read_query<account_entity, domain::account>(
        ctx,
        query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(),
        "Reading latest account by id.");
}


std::vector<domain::account> account_repository::read_all(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading all account versions. " << "id: " << id;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id) |
                       order_by("version"_c.desc(), "valid_from"_c.desc());

    return execute_read_query<account_entity, domain::account>(
        ctx,
        query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(),
        "Reading all account versions by id.");
}

std::optional<domain::account>
account_repository::read_at_version(context ctx, const std::string& id, std::uint32_t version) {
    BOOST_LOG_SEV(lg(), debug) << "Reading account at version. " << "id: " << id
                               << " version: " << version;
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_entity>> |
                       where("tenant_id"_c == tid && "id"_c == id && "version"_c == version) |
                       sqlgen::limit(1);

    const auto entities = execute_read_query<account_entity, domain::account>(
        ctx,
        query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(),
        "Reading account at version.");

    if (entities.empty())
        return std::nullopt;
    return entities.front();
}


void account_repository::remove(context ctx, const std::string& id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing account. " << "id: " << id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<account_entity> |
                       where("tenant_id"_c == tid && "id"_c == id && "valid_to"_c == max.value());

    execute_delete_query(ctx, query, lg(), "Removing account from database.");
}

std::vector<domain::account>
account_repository::read_latest(context ctx, std::uint32_t offset, std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest accounts with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<account_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    return execute_read_query<account_entity, domain::account>(
        ctx,
        query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(),
        "Reading latest accounts with pagination.");
}

std::uint32_t account_repository::get_total_account_count(context ctx) {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active account count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::select_from<account_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active account count: " << count;
    return count;
}

void account_repository::remove(context ctx, const std::vector<std::string>& ids) {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx.tenant_id().to_string();
    const auto query = sqlgen::delete_from<account_entity> |
                       where("tenant_id"_c == tid && "id"_c.in(ids) && "valid_to"_c == max.value());
    execute_delete_query(ctx, query, lg(), "Batch removing accounts.");
}


std::vector<domain::account> account_repository::read_all(context ctx) {
    const auto query = sqlgen::read<std::vector<account_entity>> | order_by("valid_from"_c.desc());

    return execute_read_query<account_entity, domain::account>(
        ctx,
        query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(),
        "Reading all accounts.");
}

std::vector<domain::account>
account_repository::read_latest_by_username(context ctx, const std::string& username) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account by username: " << username;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<account_entity>> |
                       where("username"_c == username && "valid_to"_c == max.value()) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<account_entity, domain::account>(
        ctx,
        query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(),
        "Reading latest account by username");
}

std::vector<domain::account> account_repository::read_latest_by_email(context ctx,
                                                                      const std::string& email) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest account by email: " << email;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<account_entity>> |
                       where("email"_c == email && "valid_to"_c == max.value()) |
                       order_by("valid_from"_c.desc());

    return execute_read_query<account_entity, domain::account>(
        ctx,
        query,
        [](const auto& entities) { return account_mapper::map(entities); },
        lg(),
        "Reading latest account by email");
}

std::optional<boost::uuids::uuid> account_repository::check_service_credentials(
    context ctx, const std::string& username, const std::string& password) {
    BOOST_LOG_SEV(lg(), debug) << "Checking service credentials for: " << username;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto query = sqlgen::read<std::vector<account_entity>> |
                       where("username"_c == username && "valid_to"_c == max.value()) | limit(1);

    const auto r = sqlgen::session(ctx.connection_pool()).and_then(query);
    ensure_success(r, lg());

    if (!r || r->empty())
        return std::nullopt;

    const auto& entity = r->front();
    if (entity.account_type == "user") {
        BOOST_LOG_SEV(lg(), debug) << "Rejecting user account for service login: " << username;
        return std::nullopt;
    }

    std::array<unsigned char, EVP_MAX_MD_SIZE> digest{};
    unsigned int digest_len = 0;
    EVP_Digest(password.data(), password.size(), digest.data(), &digest_len, EVP_sha256(), nullptr);

    std::string computed_hash;
    computed_hash.reserve(digest_len * 2);
    for (unsigned int i = 0; i < digest_len; ++i)
        computed_hash += std::format("{:02x}", digest[i]);

    if (!entity.service_password_hash.has_value() ||
        computed_hash != entity.service_password_hash.value()) {
        BOOST_LOG_SEV(lg(), debug) << "Password mismatch for service account: " << username;
        return std::nullopt;
    }

    return boost::lexical_cast<boost::uuids::uuid>(entity.id.value());
}

}
