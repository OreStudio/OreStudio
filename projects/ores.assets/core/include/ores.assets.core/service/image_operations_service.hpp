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
#ifndef ORES_ASSETS_CORE_SERVICE_IMAGE_OPERATIONS_SERVICE_HPP
#define ORES_ASSETS_CORE_SERVICE_IMAGE_OPERATIONS_SERVICE_HPP

#include "ores.assets.api/messaging/image_operations_protocol.hpp"
#include "ores.assets.core/export.hpp"
#include "ores.assets.core/repository/image_repository.hpp"
#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.utility/uuid/uuid_v7_generator.hpp"

namespace ores::assets::service {

/**
 * @brief Serves the image operations the CRUD surface does not carry.
 *
 * The operations differ from the entity writes: an upload states bytes and
 * their media type rather than a whole record, and the policy read answers
 * the rule the upload must satisfy. The rule itself lives in the validator,
 * so this service reads it there rather than stating it again.
 */
class ORES_ASSETS_CORE_EXPORT image_operations_service {
private:
    inline static std::string_view logger_name = "ores.assets.service.image_operations_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a image_operations_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit image_operations_service(context ctx);

    /**
     * @brief Stores an uploaded image and answers its id.
     *
     * The bytes are decoded and validated against the upload policy before
     * anything is written, so a refusal leaves the store untouched. An
     * accepted upload lands with a minted id, and the code is the text form
     * of that id, because an upload states a picture rather than a name.
     *
     * @param request The stated media type and the bytes.
     * @return The shared result, and the id of the stored image on success.
     */
    messaging::upload_image_response upload_image(const messaging::upload_image_request& request);

    /**
     * @brief Answers the rule an upload must satisfy.
     *
     * The answer is the validator's own policy record, so the rule a caller
     * shows is the rule the server applies.
     */
    messaging::get_image_upload_policy_response
    get_image_upload_policy(const messaging::get_image_upload_policy_request& request);

    /**
     * @brief Answers the caller tenant's image with a code, copying the
     *        system tenant's template when it holds none.
     *
     * Idempotent: a tenant that already holds the code is answered with what
     * it holds, and nothing is written. The copy goes through the same store
     * the upload uses, so a tenant image is a tenant image whichever path
     * made it. A code with no template is refused rather than answered with
     * nothing, so a caller that wanted a picture learns the installation does
     * not carry it.
     *
     * @param request The image code to ensure.
     * @return The shared result, and the tenant image's id on success.
     */
    messaging::ensure_image_response ensure_image(const messaging::ensure_image_request& request);

private:
    context ctx_;
    repository::image_repository repo_;
    utility::uuid::uuid_v7_generator uuid_generator_;
};

}

#endif
