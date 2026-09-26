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
#include "ores.assets.core/repository/image_tag_repository.hpp"
#include "ores.assets.api/domain/image_tag_json_io.hpp" // IWYU pragma: keep.
#include "ores.assets.core/repository/image_tag_entity.hpp"
#include "ores.assets.core/repository/image_tag_mapper.hpp"
#include "ores.database/repository/bitemporal_operations.hpp"
#include "ores.database/repository/helpers.hpp"
#include <boost/uuid/uuid_io.hpp>
#include <cstddef>
#include <optional>
#include <sqlgen/postgres.hpp>
#include <stdexcept>

namespace ores::assets::repository {

using namespace sqlgen;
using namespace sqlgen::literals;
using namespace ores::logging;
using namespace ores::database::repository;

std::string image_tag_repository::sql() {
    return generate_create_table_sql<image_tag_entity>(lg());
}

image_tag_repository::image_tag_repository(context ctx)
    : ctx_(std::move(ctx)) {}

ores::utility::domain::precondition
image_tag_repository::replace_claim(const domain::image_tag& v) {
    const auto current = read_latest(v.image_id, v.tag_id);
    if (current.empty())
        return {ores::utility::domain::precondition_kind::must_not_exist, std::nullopt};
    return {ores::utility::domain::precondition_kind::must_match_version,
            static_cast<std::uint32_t>(current.front().version)};
}

domain::image_tag
image_tag_repository::apply_claim(const domain::image_tag& v,
                                  const ores::utility::domain::precondition& claim) {
    using ores::utility::domain::precondition_kind;
    auto t = v;
    switch (claim.kind) {
        case precondition_kind::must_not_exist:
            // Zero states that no current row exists, which is the one meaning the
            // store gives a zero version.
            t.version = 0;
            break;
        case precondition_kind::must_match_version:
            t.version = claim.version ? static_cast<int>(*claim.version) : 0;
            break;
        case precondition_kind::any: {
            // A caller that claims nothing still has to say what it replaces, so
            // the row is read and its version stated. A row that moved on between
            // this read and the write is a conflict the trigger raises, never a
            // silent overwrite.
            const auto current = read_latest(v.image_id, v.tag_id);
            t.version = current.empty() ? 0 : current.front().version;
            break;
        }
    }
    return t;
}

void image_tag_repository::write(const domain::image_tag& image_tag) {
    write(image_tag, replace_claim(image_tag));
}

void image_tag_repository::write(const std::vector<domain::image_tag>& image_tags) {
    std::vector<ores::utility::domain::precondition> claims;
    claims.reserve(image_tags.size());
    for (const auto& item : image_tags)
        claims.push_back(replace_claim(item));
    write(image_tags, claims);
}

void image_tag_repository::write(const domain::image_tag& image_tag,
                                 const ores::utility::domain::precondition& claim) {
    BOOST_LOG_SEV(lg(), debug) << "Writing image tag to database: " << image_tag.image_id << "/"
                               << image_tag.tag_id;
    const auto t = apply_claim(image_tag, claim);
    execute_write_query(ctx_, image_tag_mapper::map(t), lg(), "writing image tag to database");
}

void image_tag_repository::write(const std::vector<domain::image_tag>& image_tags,
                                 const std::vector<ores::utility::domain::precondition>& claims) {
    BOOST_LOG_SEV(lg(), debug) << "Writing image tags to database. Count: " << image_tags.size();
    std::vector<domain::image_tag> batch;
    batch.reserve(image_tags.size());
    for (std::size_t i = 0; i < image_tags.size(); ++i)
        batch.push_back(apply_claim(image_tags[i], claims[i]));
    execute_write_query(ctx_, image_tag_mapper::map(batch), lg(), "writing image tags to database");
}

std::vector<domain::image_tag> image_tag_repository::read_latest() {
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<image_tag_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("image_id"_c, "tag_id"_c);

    return execute_read_query<image_tag_entity, domain::image_tag>(
        ctx_,
        query,
        [](const auto& entities) { return image_tag_mapper::map(entities); },
        lg(),
        "Reading latest image tags");
}

std::vector<domain::image_tag> image_tag_repository::read_latest(std::uint32_t offset,
                                                                 std::uint32_t limit) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest image tags with offset: " << offset
                               << " and limit: " << limit;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<image_tag_entity>> |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       order_by("image_id"_c, "tag_id"_c) | sqlgen::offset(offset) |
                       sqlgen::limit(limit);

    return execute_read_query<image_tag_entity, domain::image_tag>(
        ctx_,
        query,
        [](const auto& entities) { return image_tag_mapper::map(entities); },
        lg(),
        "Reading latest image tags (paginated).");
}

std::vector<domain::image_tag> image_tag_repository::read_latest(const boost::uuids::uuid& image_id,
                                                                 const boost::uuids::uuid& tag_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest image tag. " << image_id << "/" << tag_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto image_id_str = boost::uuids::to_string(image_id);
    const auto tag_id_str = boost::uuids::to_string(tag_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::read<std::vector<image_tag_entity>> |
                       where("tenant_id"_c == tid && "image_id"_c == image_id_str &&
                             "tag_id"_c == tag_id_str && "valid_to"_c == max.value());

    return execute_read_query<image_tag_entity, domain::image_tag>(
        ctx_,
        query,
        [](const auto& entities) { return image_tag_mapper::map(entities); },
        lg(),
        "Reading latest image tag by key.");
}

std::uint32_t image_tag_repository::get_total_image_tag_count() {
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active image tags count";
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::select_from<image_tag_entity>(sqlgen::count().as<"count">()) |
                       where("tenant_id"_c == tid && "valid_to"_c == max.value()) |
                       sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active image tags count: " << count;
    return count;
}

std::vector<domain::image_tag>
image_tag_repository::read_latest_by_image(const boost::uuids::uuid& image_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest image tags. Image: " << image_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto image_id_str = boost::uuids::to_string(image_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<image_tag_entity>> |
        where("tenant_id"_c == tid && "image_id"_c == image_id_str && "valid_to"_c == max.value()) |
        order_by("tag_id"_c);

    auto rows = execute_read_query<image_tag_entity, domain::image_tag>(
        ctx_,
        query,
        [](const auto& entities) { return image_tag_mapper::map(entities); },
        lg(),
        "Reading latest image tags by image.");

    return rows;
}

std::vector<domain::image_tag>
image_tag_repository::read_latest_by_tag(const boost::uuids::uuid& tag_id) {
    BOOST_LOG_SEV(lg(), debug) << "Reading latest image tags. Tag: " << tag_id;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tag_id_str = boost::uuids::to_string(tag_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<image_tag_entity>> |
        where("tenant_id"_c == tid && "tag_id"_c == tag_id_str && "valid_to"_c == max.value()) |
        order_by("image_id"_c);

    auto rows = execute_read_query<image_tag_entity, domain::image_tag>(
        ctx_,
        query,
        [](const auto& entities) { return image_tag_mapper::map(entities); },
        lg(),
        "Reading latest image tags by tag.");

    return rows;
}

std::vector<domain::image_tag> image_tag_repository::read_latest_by_image(
    const boost::uuids::uuid& image_id, std::uint32_t offset, std::uint32_t limit) {
    const auto image_id_str = boost::uuids::to_string(image_id);
    BOOST_LOG_SEV(lg(), debug) << "Reading latest image tags. Image: " << image_id
                               << " offset: " << offset << " limit: " << limit;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::read<std::vector<image_tag_entity>> |
        where("tenant_id"_c == tid && "image_id"_c == image_id_str && "valid_to"_c == max.value()) |
        order_by("tag_id"_c) | sqlgen::offset(offset) | sqlgen::limit(limit);

    auto rows = execute_read_query<image_tag_entity, domain::image_tag>(
        ctx_,
        query,
        [](const auto& entities) { return image_tag_mapper::map(entities); },
        lg(),
        "Reading latest image tags by image (paginated).");

    return rows;
}

std::uint32_t
image_tag_repository::get_total_image_tag_count_by_image(const boost::uuids::uuid& image_id) {
    const auto image_id_str = boost::uuids::to_string(image_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active image tags count. Image: " << image_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<image_tag_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "image_id"_c == image_id_str && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active image tags count by image: " << count;
    return count;
}

std::uint32_t
image_tag_repository::get_total_image_tag_count_by_tag(const boost::uuids::uuid& tag_id) {
    const auto tag_id_str = boost::uuids::to_string(tag_id);
    BOOST_LOG_SEV(lg(), debug) << "Retrieving total active image tags count. Tag: " << tag_id;
    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));

    struct count_result {
        long long count;
    };

    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::select_from<image_tag_entity>(sqlgen::count().as<"count">()) |
        where("tenant_id"_c == tid && "tag_id"_c == tag_id_str && "valid_to"_c == max.value()) |
        sqlgen::to<count_result>;

    const auto r = sqlgen::session(ctx_.connection_pool()).and_then(query);
    ensure_success(r, lg());

    const auto count = static_cast<std::uint32_t>(r->count);
    BOOST_LOG_SEV(lg(), debug) << "Total active image tags count by tag: " << count;
    return count;
}

void image_tag_repository::remove(const boost::uuids::uuid& image_id,
                                  const boost::uuids::uuid& tag_id) {
    static_cast<void>(remove(image_id, tag_id, std::nullopt));
}

image_tag_repository::remove_status
image_tag_repository::remove(const boost::uuids::uuid& image_id,
                             const boost::uuids::uuid& tag_id,
                             std::optional<std::uint32_t> version) {
    BOOST_LOG_SEV(lg(), debug) << "Removing image tag from database: " << image_id << "/" << tag_id;

    const auto current = read_latest(image_id, tag_id);
    if (current.empty())
        return remove_status::missing;
    // The protocol states the version as a uint32 and the row carries it as an
    // int, so the comparison states the conversion rather than relying on one.
    if (version && static_cast<std::uint32_t>(current.front().version) != *version)
        return remove_status::conflicting;

    static const auto max(make_timestamp(MAX_TIMESTAMP, lg()));
    // The row is named by its version as well as by its key, so the removal
    // cannot close a row that replaced the one the caller read between the
    // read above and this statement.
    const auto expected = version ? static_cast<int>(*version) : current.front().version;
    const auto image_id_str = boost::uuids::to_string(image_id);
    const auto tag_id_str = boost::uuids::to_string(tag_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query =
        sqlgen::delete_from<image_tag_entity> |
        where("tenant_id"_c == tid && "image_id"_c == image_id_str && "tag_id"_c == tag_id_str &&
              "valid_to"_c == max.value() && "version"_c == expected);

    execute_delete_query(ctx_, query, lg(), "removing image tag from database");
    // The delete reports no affected-row count, so the row is read back: a row
    // still open after the statement means the store refused the removal, and
    // the caller hears "conflicting" rather than "removed".
    if (!read_latest(image_id, tag_id).empty())
        return remove_status::conflicting;
    return remove_status::removed;
}

void image_tag_repository::remove(const std::vector<boost::uuids::uuid>& image_ids,
                                  const std::vector<boost::uuids::uuid>& tag_ids) {
    // A junction's key is the pair of columns, and a per-column .in() DELETE
    // would be a cross-product over-delete (rows outside the requested pairs),
    // so each pair is removed on its own.
    if (image_ids.size() != tag_ids.size())
        throw std::invalid_argument(
            "image_tag_repository::remove: key column vectors must be the same length");
    for (std::size_t i = 0; i < image_ids.size(); ++i)
        static_cast<void>(remove(image_ids[i], tag_ids[i], std::nullopt));
}

void image_tag_repository::remove_by_image(const boost::uuids::uuid& image_id) {
    BOOST_LOG_SEV(lg(), debug) << "Removing all image tags from database: " << image_id;

    const auto image_id_str = boost::uuids::to_string(image_id);
    const auto tid = ctx_.tenant_id().to_string();
    const auto query = sqlgen::delete_from<image_tag_entity> |
                       where("tenant_id"_c == tid && "image_id"_c == image_id_str);

    execute_delete_query(ctx_, query, lg(), "removing all image tags from database");
}


}
