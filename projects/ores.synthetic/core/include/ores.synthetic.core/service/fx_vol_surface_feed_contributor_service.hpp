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
#ifndef ORES_SYNTHETIC_CORE_SERVICE_FX_VOL_SURFACE_FEED_CONTRIBUTOR_SERVICE_HPP
#define ORES_SYNTHETIC_CORE_SERVICE_FX_VOL_SURFACE_FEED_CONTRIBUTOR_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.synthetic.api/domain/fx_vol_surface_feed_contributor.hpp"
#include "ores.synthetic.api/messaging/fx_vol_surface_feed_contributor_protocol.hpp"
#include "ores.synthetic.core/export.hpp"
#include "ores.synthetic.core/repository/fx_vol_surface_feed_contributor_repository.hpp"
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::synthetic::service {

/**
 * @brief Service for managing FX volatility surface feed contributors.
 *
 * Provides a higher-level interface for FX volatility surface feed contributor operations,
 * wrapping the underlying repository.
 */
class ORES_SYNTHETIC_CORE_EXPORT fx_vol_surface_feed_contributor_service {
private:
    inline static std::string_view logger_name =
        "ores.synthetic.service.fx_vol_surface_feed_contributor_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a fx_vol_surface_feed_contributor_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit fx_vol_surface_feed_contributor_service(const context& ctx);

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
    ::ores::synthetic::messaging::list_fx_vol_surface_feed_contributors_response
    list_fx_vol_surface_feed_contributors(
        const ::ores::synthetic::messaging::list_fx_vol_surface_feed_contributors_request& request);
    ::ores::synthetic::messaging::get_fx_vol_surface_feed_contributor_response
    get_fx_vol_surface_feed_contributor(
        const ::ores::synthetic::messaging::get_fx_vol_surface_feed_contributor_request& request);
    ::ores::synthetic::messaging::get_many_fx_vol_surface_feed_contributors_response
    get_many_fx_vol_surface_feed_contributors(
        const ::ores::synthetic::messaging::get_many_fx_vol_surface_feed_contributors_request&
            request);
    ::ores::synthetic::messaging::put_fx_vol_surface_feed_contributor_response
    put_fx_vol_surface_feed_contributor(
        const ::ores::synthetic::messaging::put_fx_vol_surface_feed_contributor_request& request);
    ::ores::synthetic::messaging::put_many_fx_vol_surface_feed_contributors_response
    put_many_fx_vol_surface_feed_contributors(
        const ::ores::synthetic::messaging::put_many_fx_vol_surface_feed_contributors_request&
            request);
    ::ores::synthetic::messaging::delete_fx_vol_surface_feed_contributor_response
    delete_fx_vol_surface_feed_contributor(
        const ::ores::synthetic::messaging::delete_fx_vol_surface_feed_contributor_request&
            request);
    ::ores::synthetic::messaging::delete_many_fx_vol_surface_feed_contributors_response
    delete_many_fx_vol_surface_feed_contributors(
        const ::ores::synthetic::messaging::delete_many_fx_vol_surface_feed_contributors_request&
            request);
    ::ores::synthetic::messaging::list_fx_vol_surface_feed_contributor_versions_response
    list_fx_vol_surface_feed_contributor_versions(
        const ::ores::synthetic::messaging::list_fx_vol_surface_feed_contributor_versions_request&
            request);
    ::ores::synthetic::messaging::get_fx_vol_surface_feed_contributor_version_response
    get_fx_vol_surface_feed_contributor_version(
        const ::ores::synthetic::messaging::get_fx_vol_surface_feed_contributor_version_request&
            request);
    /**@}*/

    /**
     * @brief Lists FX volatility surface feed contributors with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of FX volatility surface feed contributors for the requested page.
     */
    std::vector<domain::fx_vol_surface_feed_contributor>
    list_fx_vol_surface_feed_contributors(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active FX volatility surface feed contributors.
     *
     * @return Total number of active FX volatility surface feed contributors.
     */
    std::uint32_t count_fx_vol_surface_feed_contributors();


    /**
     * @brief Retrieves a single FX volatility surface feed contributor as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The FX volatility surface feed contributor at that version if found, std::nullopt
     * otherwise.
     */
    std::optional<domain::fx_vol_surface_feed_contributor>
    get_fx_vol_surface_feed_contributor_at_version(const boost::uuids::uuid& id,
                                                   std::uint32_t version);

    /**
     * @brief Retrieves a single FX volatility surface feed contributor by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The FX volatility surface feed contributor if found, std::nullopt otherwise.
     */
    std::optional<domain::fx_vol_surface_feed_contributor>
    get_fx_vol_surface_feed_contributor(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of FX volatility surface feed contributors by primary key.
     */
    std::vector<domain::fx_vol_surface_feed_contributor>
    get_fx_vol_surface_feed_contributors(const std::vector<std::string>& ids);

    /**
     * @brief Saves a FX volatility surface feed contributor (creates or updates).
     *
     * @param fx_vol_surface_feed_contributor The FX volatility surface feed contributor to save.
     * @throws std::exception on failure.
     */
    void save_fx_vol_surface_feed_contributor(
        const domain::fx_vol_surface_feed_contributor& fx_vol_surface_feed_contributor);

    /**
     * @brief Saves a batch of FX volatility surface feed contributors.
     *
     * @param fx_vol_surface_feed_contributors The FX volatility surface feed contributors to save.
     * @throws std::exception on failure.
     */
    void save_fx_vol_surface_feed_contributors(
        const std::vector<domain::fx_vol_surface_feed_contributor>&
            fx_vol_surface_feed_contributors);

    /**
     * @brief Deletes a FX volatility surface feed contributor by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_fx_vol_surface_feed_contributor(const boost::uuids::uuid& id);

    /**
     * @brief Deletes FX volatility surface feed contributors by their primary keys.
     */
    void delete_fx_vol_surface_feed_contributors(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a FX volatility surface feed contributor.
     *
     * Addressed by the entity's key, which is its storage key.
     */
    std::vector<domain::fx_vol_surface_feed_contributor>
    get_fx_vol_surface_feed_contributor_history(const std::string& id);

private:
    context ctx_;
    repository::fx_vol_surface_feed_contributor_repository repo_;

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
    ores::utility::domain::result prepare_change(
        const ::ores::synthetic::messaging::fx_vol_surface_feed_contributor_change& change,
        const ores::utility::domain::change_intent& intent,
        domain::fx_vol_surface_feed_contributor& out);
};

}

#endif
