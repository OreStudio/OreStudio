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
#ifndef ORES_IAM_CORE_SERVICE_IDENTITY_PROVIDER_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_IDENTITY_PROVIDER_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/identity_provider.hpp"
#include "ores.iam.api/messaging/identity_provider_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/identity_provider_repository.hpp"
#include "ores.logging/logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing identity providers.
 *
 * Provides a higher-level interface for identity provider operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT identity_provider_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.identity_provider_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a identity_provider_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit identity_provider_service(const context& ctx);

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
    ::ores::iam::messaging::list_identity_providers_response
    list_identity_providers(const ::ores::iam::messaging::list_identity_providers_request& request);
    ::ores::iam::messaging::get_identity_provider_response
    get_identity_provider(const ::ores::iam::messaging::get_identity_provider_request& request);
    ::ores::iam::messaging::get_many_identity_providers_response get_many_identity_providers(
        const ::ores::iam::messaging::get_many_identity_providers_request& request);
    ::ores::iam::messaging::put_identity_provider_response
    put_identity_provider(const ::ores::iam::messaging::put_identity_provider_request& request);
    ::ores::iam::messaging::put_many_identity_providers_response put_many_identity_providers(
        const ::ores::iam::messaging::put_many_identity_providers_request& request);
    ::ores::iam::messaging::delete_identity_provider_response delete_identity_provider(
        const ::ores::iam::messaging::delete_identity_provider_request& request);
    ::ores::iam::messaging::delete_many_identity_providers_response delete_many_identity_providers(
        const ::ores::iam::messaging::delete_many_identity_providers_request& request);
    ::ores::iam::messaging::list_identity_provider_versions_response
    list_identity_provider_versions(
        const ::ores::iam::messaging::list_identity_provider_versions_request& request);
    ::ores::iam::messaging::get_identity_provider_version_response get_identity_provider_version(
        const ::ores::iam::messaging::get_identity_provider_version_request& request);
    /**@}*/

    /**
     * @brief Lists identity providers with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of identity providers for the requested page.
     */
    std::vector<domain::identity_provider> list_providers(std::uint32_t offset,
                                                          std::uint32_t limit);

    /**
     * @brief Gets the total count of active identity providers.
     *
     * @return Total number of active identity providers.
     */
    std::uint32_t count_providers();


    /**
     * @brief Retrieves a single identity provider as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The identity provider at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::identity_provider> get_provider_at_version(const boost::uuids::uuid& id,
                                                                     std::uint32_t version);

    /**
     * @brief Retrieves a single identity provider by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The identity provider if found, std::nullopt otherwise.
     */
    std::optional<domain::identity_provider> get_provider(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single identity provider by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The identity provider if found, std::nullopt otherwise.
     */
    std::optional<domain::identity_provider> get_provider_by_code(const std::string& code);

    /**
     * @brief Retrieves a batch of identity providers by primary key.
     */
    std::vector<domain::identity_provider> get_providers(const std::vector<std::string>& ids);

    /**
     * @brief Saves a identity provider (creates or updates).
     *
     * @param provider The identity provider to save.
     * @throws std::exception on failure.
     */
    void save_provider(const domain::identity_provider& provider);

    /**
     * @brief Saves a batch of identity providers.
     *
     * @param providers The identity providers to save.
     * @throws std::exception on failure.
     */
    void save_providers(const std::vector<domain::identity_provider>& providers);

    /**
     * @brief Deletes a identity provider by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_provider(const boost::uuids::uuid& id);

    /**
     * @brief Deletes identity providers by their primary keys.
     */
    void delete_providers(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a identity provider.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::identity_provider> get_provider_history(const std::string& key);

private:
    context ctx_;
    repository::identity_provider_repository repo_;

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
    prepare_change(const ::ores::iam::messaging::identity_provider_change& change,
                   const ores::utility::domain::change_intent& intent,
                   const std::vector<domain::identity_provider>& current,
                   domain::identity_provider& out);
};

}

#endif
