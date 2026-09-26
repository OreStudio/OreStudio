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
 * Template: cpp_service.hpp.mustache
 * To modify, update the template and regenerate.
 */
#ifndef ORES_ASSETS_CORE_SERVICE_TAG_SERVICE_HPP
#define ORES_ASSETS_CORE_SERVICE_TAG_SERVICE_HPP

#include "ores.assets.api/domain/tag.hpp"
#include "ores.assets.api/messaging/tag_protocol.hpp"
#include "ores.assets.core/export.hpp"
#include "ores.assets.core/repository/tag_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::assets::service {

/**
 * @brief Service for managing asset tags.
 *
 * Provides a higher-level interface for asset tag operations,
 * wrapping the underlying repository.
 */
class ORES_ASSETS_CORE_EXPORT tag_service {
private:
    inline static std::string_view logger_name = "ores.assets.service.tag_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a tag_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit tag_service(context ctx);

    /**
     * @brief The protocol operations, one method per subject.
     *
     * A method takes the canonical request and answers its response, so the
     * handler that serves the subject decodes, calls and replies without
     * deciding anything. The result a caller reads -- missing, conflicting,
     * denied -- is filled here, where the storage call that decided it is
     * made, rather than being inferred from an exception.
     */
    /**@{*/
    messaging::list_tags_response list_tags(const messaging::list_tags_request& request);
    messaging::get_tag_response get_tag(const messaging::get_tag_request& request);
    messaging::get_many_tags_response
    get_many_tags(const messaging::get_many_tags_request& request);
    messaging::put_tag_response put_tag(const messaging::put_tag_request& request);
    messaging::put_many_tags_response
    put_many_tags(const messaging::put_many_tags_request& request);
    messaging::delete_tag_response delete_tag(const messaging::delete_tag_request& request);
    messaging::delete_many_tags_response
    delete_many_tags(const messaging::delete_many_tags_request& request);
    messaging::list_tag_versions_response
    list_tag_versions(const messaging::list_tag_versions_request& request);
    messaging::get_tag_version_response
    get_tag_version(const messaging::get_tag_version_request& request);
    /**@}*/

    /**
     * @brief Lists asset tags with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of asset tags for the requested page.
     */
    std::vector<domain::tag> list_tags(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active asset tags.
     *
     * @return Total number of active asset tags.
     */
    std::uint32_t count_tags();


    /**
     * @brief Retrieves a single asset tag as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The asset tag at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::tag> get_tag_at_version(const boost::uuids::uuid& id,
                                                  std::uint32_t version);

    /**
     * @brief Retrieves a single asset tag by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The asset tag if found, std::nullopt otherwise.
     */
    std::optional<domain::tag> get_tag(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single asset tag by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The asset tag if found, std::nullopt otherwise.
     */
    std::optional<domain::tag> get_tag_by_name(const std::string& name);

    /**
     * @brief Retrieves a batch of asset tags by primary key.
     */
    std::vector<domain::tag> get_tags(const std::vector<std::string>& ids);

    /**
     * @brief Saves a asset tag (creates or updates).
     *
     * @param tag The asset tag to save.
     * @throws std::exception on failure.
     */
    void save_tag(const domain::tag& tag);

    /**
     * @brief Saves a batch of asset tags.
     *
     * @param tags The asset tags to save.
     * @throws std::exception on failure.
     */
    void save_tags(const std::vector<domain::tag>& tags);

    /**
     * @brief Deletes a asset tag by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_tag(const boost::uuids::uuid& id);

    /**
     * @brief Deletes asset tags by their primary keys.
     */
    void delete_tags(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a asset tag.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::tag> get_tag_history(const std::string& key);

private:
    context ctx_;
    repository::tag_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result prepare_change(const messaging::tag_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::tag& out);
};

}

#endif
