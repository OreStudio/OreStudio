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
#include "ores.qt/HostDisplayNameCache.hpp"

#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

HostDisplayNameCache::HostDisplayNameCache(QObject* parent)
    : QObject(parent) {}

QString HostDisplayNameCache::display_name_for(const QString& uuid) const {
    const auto it = cache_.find(uuid);
    if (it != cache_.end() && !it->isEmpty())
        return *it;
    // Short-ID fallback: first 8 hex characters (before the first '-').
    return uuid.left(8);
}

void HostDisplayNameCache::populate_from(
    const std::vector<compute::domain::host>& hosts)
{
    for (const auto& h : hosts) {
        const auto uuid = QString::fromStdString(
            boost::uuids::to_string(h.id));
        if (!h.display_name.empty())
            cache_.insert(uuid, QString::fromStdString(h.display_name));
    }
}

void HostDisplayNameCache::clear() {
    cache_.clear();
}

} // namespace ores::qt
