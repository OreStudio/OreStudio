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
#ifndef ORES_MARKETDATA_SERVICE_MARKET_SERIES_ASSET_CLASS_SERVICE_HPP
#define ORES_MARKETDATA_SERVICE_MARKET_SERIES_ASSET_CLASS_SERVICE_HPP

#include "ores.database/domain/context.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.marketdata.api/domain/market_series_asset_class.hpp"
#include "ores.marketdata.api/messaging/market_series_asset_class_protocol.hpp"
#include "ores.marketdata.core/repository/market_series_asset_class_repository.hpp"
#include <boost/uuid/uuid.hpp>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

namespace ores::marketdata::service {

/**
 * @brief Service for managing asset classes.
 *
 * Provides a higher-level interface for asset class operations,
 * wrapping the underlying repository.
 */
class market_series_asset_class_service {
private:
    inline static std::string_view logger_name =
        "ores.marketdata.service.market_series_asset_class_service";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    using context = ores::database::context;

    /**
     * @brief Constructs a market_series_asset_class_service with a database context.
     *
     * @param ctx The database context for operations.
     */
    explicit market_series_asset_class_service(context ctx);

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
    messaging::list_market_series_asset_classes_response list_market_series_asset_classes(
        const messaging::list_market_series_asset_classes_request& request);
    messaging::get_market_series_asset_class_response
    get_market_series_asset_class(const messaging::get_market_series_asset_class_request& request);
    messaging::get_many_market_series_asset_classes_response get_many_market_series_asset_classes(
        const messaging::get_many_market_series_asset_classes_request& request);
    messaging::put_market_series_asset_class_response
    put_market_series_asset_class(const messaging::put_market_series_asset_class_request& request);
    messaging::put_many_market_series_asset_classes_response put_many_market_series_asset_classes(
        const messaging::put_many_market_series_asset_classes_request& request);
    messaging::delete_market_series_asset_class_response delete_market_series_asset_class(
        const messaging::delete_market_series_asset_class_request& request);
    messaging::delete_many_market_series_asset_classes_response
    delete_many_market_series_asset_classes(
        const messaging::delete_many_market_series_asset_classes_request& request);
    messaging::list_by_market_series_id_market_series_asset_classes_response
    list_by_market_series_id_market_series_asset_classes(
        const messaging::list_by_market_series_id_market_series_asset_classes_request& request);
    /**@}*/

private:
    context ctx_;
    repository::market_series_asset_class_repository repo_;

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
    ores::utility::domain::result
    prepare_change(const messaging::market_series_asset_class_change& change,
                   const ores::utility::domain::change_intent& intent,
                   domain::market_series_asset_class& out);
};

}

#endif
