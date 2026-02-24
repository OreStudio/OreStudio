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
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/ImageCache.hpp"

namespace ores::qt {

void apply_flag_icons(QComboBox* combo, ImageCache* cache, FlagSource source) {
    if (!combo || !cache) return;

    auto resolver = [cache, source](const std::string& code) -> QIcon {
        switch (source) {
        case FlagSource::Currency:
            return cache->getCurrencyFlagIcon(code);
        case FlagSource::Country:
            return cache->getCountryFlagIcon(code);
        case FlagSource::BusinessCentre:
            return cache->getBusinessCentreFlagIcon(code);
        }
        return {};
    };

    set_combo_flag_icons(combo, resolver);
}

void setup_flag_combo(
    QObject* context, QComboBox* combo, ImageCache* cache, FlagSource source) {
    if (!combo || !cache) return;

    apply_flag_icons(combo, cache, source);

    // Re-apply when the full image set arrives from the server.
    QObject::connect(cache, &ImageCache::allLoaded, context, [combo, cache, source]() {
        apply_flag_icons(combo, cache, source);
    });
}

}
