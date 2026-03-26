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
#ifndef ORES_QT_HOST_DISPLAY_NAME_CACHE_HPP
#define ORES_QT_HOST_DISPLAY_NAME_CACHE_HPP

#include <vector>
#include <QHash>
#include <QObject>
#include <QString>
#include "ores.compute.api/domain/host.hpp"

namespace ores::qt {

/**
 * @brief Shared cache mapping host UUID strings to whimsical display names.
 *
 * Avoids repeated list-hosts requests by centralising the UUID → name
 * mapping. All client models that need to render host names hold a pointer
 * to the single instance owned by the controller.
 *
 * If a UUID is not present in the cache, display_name_for() returns the
 * first 8 characters of the UUID as a short-ID fallback.
 */
class HostDisplayNameCache : public QObject {
    Q_OBJECT

public:
    explicit HostDisplayNameCache(QObject* parent = nullptr);

    /**
     * @brief Returns the display name for the given UUID string.
     *
     * Falls back to the first 8 characters of the UUID when the cache
     * has no entry (e.g., the host was registered after the last refresh).
     */
    [[nodiscard]] QString display_name_for(const QString& uuid) const;

    /**
     * @brief Populates (or updates) the cache from a list of hosts.
     *
     * Existing entries are overwritten if the host appears in @p hosts.
     * Entries for hosts not present in @p hosts are kept (no eviction).
     */
    void populate_from(const std::vector<compute::domain::host>& hosts);

    /**
     * @brief Removes all entries from the cache.
     */
    void clear();

private:
    QHash<QString, QString> cache_; // UUID string → display name
};

} // namespace ores::qt

#endif
