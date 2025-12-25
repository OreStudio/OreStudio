/* -*- mode: c++; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 *
 * Copyright (C) 2025 Marco Craveiro <marco.craveiro@gmail.com>
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
#include "ores.utility/geo/geolocation_service.hpp"

#include <maxminddb.h>

namespace ores::utility::geo {

using namespace ores::telemetry::log;

struct geolocation_service::impl {
    MMDB_s mmdb;
    bool loaded = false;
};

geolocation_service::geolocation_service()
    : pimpl_(std::make_unique<impl>()) {}

geolocation_service::geolocation_service(const std::string& database_path)
    : pimpl_(std::make_unique<impl>()) {
    load(database_path);
}

geolocation_service::~geolocation_service() {
    if (pimpl_ && pimpl_->loaded) {
        MMDB_close(&pimpl_->mmdb);
    }
}

geolocation_service::geolocation_service(geolocation_service&&) noexcept = default;
geolocation_service& geolocation_service::operator=(geolocation_service&&) noexcept = default;

bool geolocation_service::load(const std::string& database_path) {
    // Close any previously loaded database
    if (pimpl_->loaded) {
        MMDB_close(&pimpl_->mmdb);
        pimpl_->loaded = false;
    }

    int status = MMDB_open(database_path.c_str(), MMDB_MODE_MMAP, &pimpl_->mmdb);
    if (status != MMDB_SUCCESS) {
        BOOST_LOG_SEV(lg(), error)
            << "Failed to load MaxMind database: " << database_path
            << ", error: " << MMDB_strerror(status);
        return false;
    }

    pimpl_->loaded = true;
    BOOST_LOG_SEV(lg(), info) << "Loaded MaxMind database: " << database_path
                              << ", type: " << pimpl_->mmdb.metadata.database_type;
    return true;
}

bool geolocation_service::is_loaded() const {
    return pimpl_ && pimpl_->loaded;
}

namespace {

std::optional<std::string> get_string_value(MMDB_entry_data_s* entry_data) {
    if (entry_data->has_data && entry_data->type == MMDB_DATA_TYPE_UTF8_STRING) {
        return std::string(entry_data->utf8_string, entry_data->data_size);
    }
    return std::nullopt;
}

std::optional<double> get_double_value(MMDB_entry_data_s* entry_data) {
    if (entry_data->has_data && entry_data->type == MMDB_DATA_TYPE_DOUBLE) {
        return entry_data->double_value;
    }
    return std::nullopt;
}

}

std::expected<geolocation_result, geolocation_error>
geolocation_service::lookup(const boost::asio::ip::address& ip) const {
    return lookup(ip.to_string());
}

std::expected<geolocation_result, geolocation_error>
geolocation_service::lookup(const std::string& ip_string) const {
    if (!is_loaded()) {
        return std::unexpected(geolocation_error::database_not_loaded);
    }

    int gai_error = 0;
    int mmdb_error = 0;

    MMDB_lookup_result_s result = MMDB_lookup_string(
        &pimpl_->mmdb, ip_string.c_str(), &gai_error, &mmdb_error);

    if (gai_error != 0) {
        BOOST_LOG_SEV(lg(), debug) << "Invalid IP address: " << ip_string;
        return std::unexpected(geolocation_error::invalid_address);
    }

    if (mmdb_error != MMDB_SUCCESS) {
        BOOST_LOG_SEV(lg(), debug) << "MMDB lookup error for " << ip_string
                                   << ": " << MMDB_strerror(mmdb_error);
        return std::unexpected(geolocation_error::lookup_failed);
    }

    if (!result.found_entry) {
        BOOST_LOG_SEV(lg(), debug) << "No entry found for IP: " << ip_string;
        return std::unexpected(geolocation_error::address_not_found);
    }

    geolocation_result geo_result;
    MMDB_entry_data_s entry_data;
    int status;

    // Get country ISO code
    status = MMDB_get_value(&result.entry, &entry_data,
        "country", "iso_code", NULL);
    if (status == MMDB_SUCCESS) {
        if (auto val = get_string_value(&entry_data)) {
            geo_result.country_code = *val;
        }
    }

    // Get city name (English)
    status = MMDB_get_value(&result.entry, &entry_data,
        "city", "names", "en", NULL);
    if (status == MMDB_SUCCESS) {
        if (auto val = get_string_value(&entry_data)) {
            geo_result.city = *val;
        }
    }

    // Get latitude
    status = MMDB_get_value(&result.entry, &entry_data,
        "location", "latitude", NULL);
    if (status == MMDB_SUCCESS) {
        geo_result.latitude = get_double_value(&entry_data);
    }

    // Get longitude
    status = MMDB_get_value(&result.entry, &entry_data,
        "location", "longitude", NULL);
    if (status == MMDB_SUCCESS) {
        geo_result.longitude = get_double_value(&entry_data);
    }

    BOOST_LOG_SEV(lg(), trace) << "Geolocation for " << ip_string
                               << ": country=" << geo_result.country_code
                               << ", city=" << geo_result.city;

    return geo_result;
}

}
