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
#ifndef ORES_ASSETS_CORE_SERVICE_IMAGE_SERVICE_HPP
#define ORES_ASSETS_CORE_SERVICE_IMAGE_SERVICE_HPP

#include "ores.assets.api/domain/image.hpp"
#include "ores.assets.api/messaging/image_protocol.hpp"
#include "ores.assets.core/export.hpp"
#include "ores.assets.core/repository/image_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::assets::service {

/**
 * @brief Service for managing asset images.
 *
 * Provides a higher-level interface for asset image operations,
 * wrapping the underlying repository.
 */
class ORES_ASSETS_CORE_EXPORT image_service {
private:
    inline static std::string_view logger_name = "ores.assets.service.image_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a image_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit image_service(context ctx);

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
    messaging::list_images_response list_images(const messaging::list_images_request& request);
    messaging::get_image_response get_image(const messaging::get_image_request& request);
    messaging::get_many_images_response
    get_many_images(const messaging::get_many_images_request& request);
    messaging::put_image_response put_image(const messaging::put_image_request& request);
    messaging::put_many_images_response
    put_many_images(const messaging::put_many_images_request& request);
    messaging::delete_image_response delete_image(const messaging::delete_image_request& request);
    messaging::delete_many_images_response
    delete_many_images(const messaging::delete_many_images_request& request);
    messaging::list_image_versions_response
    list_image_versions(const messaging::list_image_versions_request& request);
    messaging::get_image_version_response
    get_image_version(const messaging::get_image_version_request& request);
    /**@}*/

    /**
     * @brief Lists asset images with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of asset images for the requested page.
     */
    std::vector<domain::image> list_images(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active asset images.
     *
     * @return Total number of active asset images.
     */
    std::uint32_t count_images();


    /**
     * @brief Retrieves a single asset image as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The asset image at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::image> get_image_at_version(const boost::uuids::uuid& id,
                                                      std::uint32_t version);

    /**
     * @brief Retrieves a single asset image by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The asset image if found, std::nullopt otherwise.
     */
    std::optional<domain::image> get_image(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single asset image by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The asset image if found, std::nullopt otherwise.
     */
    std::optional<domain::image> get_image_by_code(const std::string& code);

    /**
     * @brief Retrieves a batch of asset images by primary key.
     */
    std::vector<domain::image> get_images(const std::vector<std::string>& ids);

    /**
     * @brief Saves a asset image (creates or updates).
     *
     * @param image The asset image to save.
     * @throws std::exception on failure.
     */
    void save_image(const domain::image& image);

    /**
     * @brief Saves a batch of asset images.
     *
     * @param images The asset images to save.
     * @throws std::exception on failure.
     */
    void save_images(const std::vector<domain::image>& images);

    /**
     * @brief Deletes a asset image by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_image(const boost::uuids::uuid& id);

    /**
     * @brief Deletes asset images by their primary keys.
     */
    void delete_images(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a asset image.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::image> get_image_history(const std::string& key);

private:
    context ctx_;
    repository::image_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::image_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::image& out);
};

}

#endif
