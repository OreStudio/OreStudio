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
#ifndef ORES_IAM_CORE_SERVICE_SEED_PROFILE_SERVICE_HPP
#define ORES_IAM_CORE_SERVICE_SEED_PROFILE_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.iam.api/domain/seed_profile.hpp"
#include "ores.iam.api/messaging/seed_profile_protocol.hpp"
#include "ores.iam.core/export.hpp"
#include "ores.iam.core/repository/seed_profile_repository.hpp"
#include "ores.logging/make_logger.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::iam::service {

/**
 * @brief Service for managing seed profiles.
 *
 * Provides a higher-level interface for seed profile operations,
 * wrapping the underlying repository.
 */
class ORES_IAM_CORE_EXPORT seed_profile_service {
private:
    inline static std::string_view logger_name = "ores.iam.service.seed_profile_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a seed_profile_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit seed_profile_service(context ctx);

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
    messaging::list_seed_profiles_response
    list_seed_profiles(const messaging::list_seed_profiles_request& request);
    messaging::get_seed_profile_response
    get_seed_profile(const messaging::get_seed_profile_request& request);
    messaging::get_many_seed_profiles_response
    get_many_seed_profiles(const messaging::get_many_seed_profiles_request& request);
    messaging::put_seed_profile_response
    put_seed_profile(const messaging::put_seed_profile_request& request);
    messaging::put_many_seed_profiles_response
    put_many_seed_profiles(const messaging::put_many_seed_profiles_request& request);
    messaging::delete_seed_profile_response
    delete_seed_profile(const messaging::delete_seed_profile_request& request);
    messaging::delete_many_seed_profiles_response
    delete_many_seed_profiles(const messaging::delete_many_seed_profiles_request& request);
    messaging::list_seed_profile_versions_response
    list_seed_profile_versions(const messaging::list_seed_profile_versions_request& request);
    messaging::get_seed_profile_version_response
    get_seed_profile_version(const messaging::get_seed_profile_version_request& request);
    /**@}*/

    /**
     * @brief Lists seed profiles with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of seed profiles for the requested page.
     */
    std::vector<domain::seed_profile> list_seed_profiles(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active seed profiles.
     *
     * @return Total number of active seed profiles.
     */
    std::uint32_t count_seed_profiles();


    /**
     * @brief Retrieves a single seed profile as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The seed profile at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::seed_profile> get_seed_profile_at_version(const boost::uuids::uuid& id,
                                                                    std::uint32_t version);

    /**
     * @brief Retrieves a single seed profile by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The seed profile if found, std::nullopt otherwise.
     */
    std::optional<domain::seed_profile> get_seed_profile(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of seed profiles by primary key.
     */
    std::vector<domain::seed_profile> get_seed_profiles(const std::vector<std::string>& ids);

    /**
     * @brief Saves a seed profile (creates or updates).
     *
     * @param seed_profile The seed profile to save.
     * @throws std::exception on failure.
     */
    void save_seed_profile(const domain::seed_profile& seed_profile);

    /**
     * @brief Saves a batch of seed profiles.
     *
     * @param seed_profiles The seed profiles to save.
     * @throws std::exception on failure.
     */
    void save_seed_profiles(const std::vector<domain::seed_profile>& seed_profiles);

    /**
     * @brief Deletes a seed profile by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_seed_profile(const boost::uuids::uuid& id);

    /**
     * @brief Deletes seed profiles by their primary keys.
     */
    void delete_seed_profiles(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a seed profile.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::seed_profile> get_seed_profile_history(const std::string& id);

private:
    context ctx_;
    repository::seed_profile_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::seed_profile_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::seed_profile& out);
};

}

#endif
