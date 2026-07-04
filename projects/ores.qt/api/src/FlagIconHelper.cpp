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

QIcon currency_flag_icon(ImageCache& imageCache,
                         const std::string& isoCode,
                         const std::string& quoteIsoCode) {
    const QIcon baseIcon = imageCache.getCurrencyFlagIcon(isoCode);
    if (quoteIsoCode.empty())
        return baseIcon;

    // Composite at every size the cache already rendered for a single flag
    // (see IconUtils::svgDataToIcon's {16,20,24,32,48} ladder), reusing those
    // exact pixmaps — not a separately-chosen size — so the pair icon is
    // built from the identical per-size images a single flag uses, just two
    // of them instead of one. Qt then auto-picks the right rung for whatever
    // iconSize the view asks for, exactly as it would for a single flag.
    const QIcon quoteIcon = imageCache.getCurrencyFlagIcon(quoteIsoCode);
    QIcon combined;
    for (const QSize& baseSize : baseIcon.availableSizes()) {
        const QPixmap basePm = baseIcon.pixmap(baseSize);
        // Request by height only (a very wide box) so this picks the quote
        // icon's own same-height rung rather than rescaling to baseSize's
        // (possibly different, if the two flags' SVGs differ in aspect
        // ratio) width.
        const QPixmap quotePm = quoteIcon.pixmap(QSize(8192, baseSize.height()));
        QPixmap out(basePm.width() + flag_spacing + quotePm.width(), baseSize.height());
        out.fill(Qt::transparent);
        QPainter painter(&out);
        painter.drawPixmap(0, 0, basePm);
        painter.drawPixmap(basePm.width() + flag_spacing, 0, quotePm);
        painter.end();
        combined.addPixmap(out);
    }
    return combined;
}

QSize currency_pair_icon_size(int flagHeight) {
    return {flagHeight * 2 + flag_spacing, flagHeight};
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
