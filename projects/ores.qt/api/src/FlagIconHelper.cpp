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
#include <QPainter>
#include <QPixmap>

namespace ores::qt {

namespace {
constexpr int flag_spacing = 2; // small gap so the two flags read as distinct, not overlapping
}

QIcon pair_flag_icon(ImageCache& imageCache,
                     const std::string& baseIsoCode,
                     const std::string& quoteIsoCode,
                     int flagSize) {
    // getCurrencyFlagPixmap renders from the cached SVG at the exact requested
    // size (crisp), falling back to the icon ladder + async load if the SVG
    // isn't cached yet — see its doc comment.
    const QPixmap basePm = imageCache.getCurrencyFlagPixmap(baseIsoCode, flagSize);
    const QPixmap quotePm = imageCache.getCurrencyFlagPixmap(quoteIsoCode, flagSize);
    const QSize size = pair_flag_icon_size(flagSize);
    QPixmap combined(size);
    combined.fill(Qt::transparent);
    QPainter painter(&combined);
    painter.drawPixmap(0, 0, basePm);
    painter.drawPixmap(flagSize + flag_spacing, 0, quotePm);
    painter.end();
    return QIcon(combined);
}

QSize pair_flag_icon_size(int flagSize) {
    return {flagSize * 2 + flag_spacing, flagSize};
}

void apply_flag_icons(QComboBox* combo, ImageCache* cache, FlagSource source) {
    if (!combo || !cache)
        return;

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

void setup_flag_combo(QObject* context, QComboBox* combo, ImageCache* cache, FlagSource source) {
    if (!combo || !cache)
        return;

    apply_flag_icons(combo, cache, source);

    // Re-apply when the full image set arrives from the server.
    QObject::connect(cache, &ImageCache::allLoaded, context, [combo, cache, source]() {
        apply_flag_icons(combo, cache, source);
    });
}

}
