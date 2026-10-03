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
#ifndef ORES_REFDATA_CORE_SERVICE_PORTFOLIO_SERVICE_HPP
#define ORES_REFDATA_CORE_SERVICE_PORTFOLIO_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata.api/domain/portfolio.hpp"
#include "ores.refdata.api/messaging/portfolio_protocol.hpp"
#include "ores.refdata.core/export.hpp"
#include "ores.refdata.core/repository/portfolio_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <chrono>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::refdata::service {

/**
 * @brief Service for managing portfolios.
 *
 * Provides a higher-level interface for portfolio operations,
 * wrapping the underlying repository.
 */
class ORES_REFDATA_CORE_EXPORT portfolio_service {
private:
    inline static std::string_view logger_name = "ores.refdata.service.portfolio_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a portfolio_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit portfolio_service(context ctx);

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
    messaging::list_portfolios_response
    list_portfolios(const messaging::list_portfolios_request& request);
    messaging::get_portfolio_response
    get_portfolio(const messaging::get_portfolio_request& request);
    messaging::get_many_portfolios_response
    get_many_portfolios(const messaging::get_many_portfolios_request& request);
    messaging::put_portfolio_response
    put_portfolio(const messaging::put_portfolio_request& request);
    messaging::put_many_portfolios_response
    put_many_portfolios(const messaging::put_many_portfolios_request& request);
    messaging::delete_portfolio_response
    delete_portfolio(const messaging::delete_portfolio_request& request);
    messaging::delete_many_portfolios_response
    delete_many_portfolios(const messaging::delete_many_portfolios_request& request);
    messaging::list_by_sandbox_id_portfolios_response
    list_by_sandbox_id_portfolios(const messaging::list_by_sandbox_id_portfolios_request& request);
    messaging::list_portfolio_versions_response
    list_portfolio_versions(const messaging::list_portfolio_versions_request& request);
    messaging::get_portfolio_version_response
    get_portfolio_version(const messaging::get_portfolio_version_request& request);
    /**@}*/

    /**
     * @brief Lists portfolios with pagination support.
     *
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of portfolios for the requested page.
     */
    std::vector<domain::portfolio> list_portfolios(std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active portfolios.
     *
     * @return Total number of active portfolios.
     */
    std::uint32_t count_portfolios();


    /**
     * @brief Lists portfolios filtered by sandbox_id, with pagination.
     *
     * @param sandbox_id The sandbox_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching portfolios for the requested page.
     */
    std::vector<domain::portfolio> list_portfolios_by_sandbox_id(const std::string& sandbox_id,
                                                                 std::uint32_t offset,
                                                                 std::uint32_t limit);

    /**
     * @brief Gets the total count of active portfolios filtered by sandbox_id.
     *
     * @param sandbox_id The sandbox_id to filter by.
     * @return Total number of matching portfolios.
     */
    std::uint32_t count_portfolios_by_sandbox_id(const std::string& sandbox_id);

    /**
     * @brief Lists portfolios filtered by sandbox_id, with pagination.
     *
     * @param sandbox_id The sandbox_id to filter by.
     * @param offset Number of records to skip.
     * @param limit Maximum number of records to return.
     * @return Vector of matching portfolios for the requested page.
     */
    std::vector<domain::portfolio> list_portfolios_by_sandbox_id(
        const boost::uuids::uuid& sandbox_id, std::uint32_t offset, std::uint32_t limit);

    /**
     * @brief Gets the total count of active portfolios filtered by sandbox_id.
     *
     * @param sandbox_id The sandbox_id to filter by.
     * @return Total number of matching portfolios.
     */
    std::uint32_t count_portfolios_by_sandbox_id(const boost::uuids::uuid& sandbox_id);


    /**
     * @brief Retrieves a single portfolio as it stood at a specific
     * version. See the "Temporal composite entity versioning" architecture doc.
     *
     * @param version The version to fetch.
     * @return The portfolio at that version if found, std::nullopt otherwise.
     */
    std::optional<domain::portfolio> get_portfolio_at_version(const boost::uuids::uuid& id,
                                                              std::uint32_t version);

    /**
     * @brief Retrieves a single portfolio by its primary key.
     *
     * The storage key is a uuid, so the signature says which key is meant and
     * the human-readable key cannot be passed here by mistake.
     *
     * @return The portfolio if found, std::nullopt otherwise.
     */
    std::optional<domain::portfolio> get_portfolio(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a single portfolio by the key the model
     * declares -- the human-readable key a caller holds.
     *
     * This is the counterpart of the uuid overload above: the two keys an
     * entity holds are different keys, and a call site has to say which one it
     * means.
     *
     * @return The portfolio if found, std::nullopt otherwise.
     */
    std::optional<domain::portfolio> get_portfolio_by_name(const std::string& name);

    /**
     * @brief Retrieves a single portfolio by its uuid primary key.
     *
     * @return The portfolio if found, std::nullopt otherwise.
     */
    std::optional<domain::portfolio> find_portfolio(const boost::uuids::uuid& id);

    /**
     * @brief Retrieves a batch of portfolios by primary key.
     */
    std::vector<domain::portfolio> get_portfolios(const std::vector<std::string>& ids);

    /**
     * @brief Saves a portfolio (creates or updates).
     *
     * @param portfolio The portfolio to save.
     * @throws std::exception on failure.
     */
    void save_portfolio(const domain::portfolio& portfolio);

    /**
     * @brief Saves a batch of portfolios.
     *
     * @param portfolios The portfolios to save.
     * @throws std::exception on failure.
     */
    void save_portfolios(const std::vector<domain::portfolio>& portfolios);

    /**
     * @brief Deletes a portfolio by its primary key.
     *
     * @throws std::exception on failure.
     */
    void delete_portfolio(const boost::uuids::uuid& id);

    /**
     * @brief Removes a portfolio by its uuid primary key.
     *
     * @throws std::exception on failure.
     */
    void remove_portfolio(const boost::uuids::uuid& id);

    /**
     * @brief Deletes portfolios by their primary keys.
     */
    void delete_portfolios(const std::vector<std::string>& ids);

    /**
     * @brief Retrieves all historical versions of a portfolio.
     *
     * Addressed by the key the model declares, which is the one a caller
     * holds; the storage key is resolved from it here, the same step every
     * other read makes.
     */
    std::vector<domain::portfolio> get_portfolio_history(const std::string& key);

    /**
     * @brief Retrieves all historical versions of a portfolio
     * by its uuid primary key.
     */
    std::vector<domain::portfolio> get_portfolio_history(const boost::uuids::uuid& id);

private:
    context ctx_;
    repository::portfolio_repository repo_;

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
    ores::utility::domain::result prepare_change(const messaging::portfolio_change& change,
                                                 const ores::utility::domain::change_intent& intent,
                                                 domain::portfolio& out);
};

}

#endif
