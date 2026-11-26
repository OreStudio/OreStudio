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
#ifndef ORES_VAULT_CORE_SERVICE_SECRET_USE_SERVICE_HPP
#define ORES_VAULT_CORE_SERVICE_SECRET_USE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/logger.hpp"
#include "ores.vault.api/domain/secret_use.hpp"
#include "ores.vault.api/messaging/secret_use_protocol.hpp"
#include "ores.vault.core/export.hpp"
#include "ores.vault.core/repository/secret_use_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::vault::service {

/**
 * @brief Service for managing uses.
 *
 * Provides a higher-level interface for use operations,
 * wrapping the underlying repository.
 */
class ORES_VAULT_CORE_EXPORT secret_use_service {
private:
    inline static std::string_view logger_name = "ores.vault.service.secret_use_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a secret_use_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit secret_use_service(const context& ctx);

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
    ::ores::vault::messaging::list_secret_uses_response
    list_secret_uses(const ::ores::vault::messaging::list_secret_uses_request& request);
    ::ores::vault::messaging::get_secret_use_response
    get_secret_use(const ::ores::vault::messaging::get_secret_use_request& request);
    ::ores::vault::messaging::get_many_secret_uses_response
    get_many_secret_uses(const ::ores::vault::messaging::get_many_secret_uses_request& request);
    ::ores::vault::messaging::put_secret_use_response
    put_secret_use(const ::ores::vault::messaging::put_secret_use_request& request);
    ::ores::vault::messaging::put_many_secret_uses_response
    put_many_secret_uses(const ::ores::vault::messaging::put_many_secret_uses_request& request);
    ::ores::vault::messaging::delete_secret_use_response
    delete_secret_use(const ::ores::vault::messaging::delete_secret_use_request& request);
    ::ores::vault::messaging::delete_many_secret_uses_response delete_many_secret_uses(
        const ::ores::vault::messaging::delete_many_secret_uses_request& request);
    /**@}*/

    /**
     * @brief Lists uses with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of uses for the requested page.
     */
    std::vector<domain::secret_use> list_secret_uses(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active uses.
     *
     * @return Total number of active uses.
     */
    std::uint32_t count_secret_uses();


    /**
     * @brief Retrieves a single use by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The use if found, std::nullopt otherwise.
     */
    std::optional<domain::secret_use> get_secret_use(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of uses by primary key.
     */
    std::vector<domain::secret_use> get_secret_uses(const std::vector<std::string>& ids);

    /**
     * @brief Saves a use (creates or updates).
     *
     * @param secret_use The use to save.
     * @throws std::exception on failure.
     */
    void save_secret_use(const domain::secret_use& secret_use);

    /**
     * @brief Saves a batch of uses.
     *
     * @param secret_uses The uses to save.
     * @throws std::exception on failure.
     */
    void save_secret_uses(const std::vector<domain::secret_use>& secret_uses);

    /**
     * @brief Deletes a use by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_secret_use(const boost::uuids::uuid& id);

    /**
     * @brief Deletes uses by their primary keys.
     */
    void delete_secret_uses(const std::vector<std::string>& ids);


private:
    context ctx_;
    repository::secret_use_repository repo_;

    /**
     * @brief Checks one change against the row it names, and stamps it.
     *
     * A single write and a batch state the same claim, so the check, the
     * server-derived provenance and the version the store must match are one
     * decision made in one place. A batch that made the decision per element
     * would eventually make it differently from the single write.
     *
     * The caller reads the current row, so a batch reads every row it names
     * in one query rather than one query per change.
     *
     * @param change The change as the caller stated it.
     * @param intent The reason and commentary the caller gave.
     * @param current The row the change names as it is now, or empty.
     * @param out The stamped domain object, written only when the result is ok.
     * @return ok, or why the change was refused.
     */
    ores::utility::domain::result
    prepare_change(const ::ores::vault::messaging::secret_use_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::secret_use>& current,
                   domain::secret_use& out);
};

}

#endif
