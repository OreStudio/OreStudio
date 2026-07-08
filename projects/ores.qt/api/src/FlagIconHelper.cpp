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
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include <QFutureWatcher>
#include <QPainter>
#include <QPixmap>
#include <QtConcurrent>
#include <algorithm>

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

QSize single_flag_icon_size(int flagHeight) {
    return {flagHeight, flagHeight};
}

QSize currency_pair_icon_size(int flagHeight) {
    return {flagHeight * 2 + flag_spacing, flagHeight};
}

QIcon currency_flag_icon(ImageCache& imageCache, const std::optional<std::string>& isoCode) {
    if (!isoCode)
        return {};
    return currency_flag_icon(imageCache, *isoCode);
}

QIcon currency_flag_icon_from_pair_code(ImageCache& imageCache, const std::string& pairCode) {
    const auto sep = pairCode.find('/');
    if (sep == std::string::npos)
        return {};
    return currency_flag_icon(
        imageCache, pairCode.substr(0, sep), pairCode.substr(sep + 1));
}

void apply_flag_icons(QComboBox* combo, ImageCache* cache, FlagSource source, QSize iconSize) {
    if (!combo || !cache)
        return;

    // Same reasoning as single_flag_icon_size()'s doc comment: an unset
    // iconSize falls back to a style-dependent default that isn't
    // guaranteed to render at a usable/visible size — every flag-bearing
    // combo must set this explicitly, not just table views.
    combo->setIconSize(iconSize);

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
    QObject* context, QComboBox* combo, ImageCache* cache, FlagSource source, QSize iconSize) {
    if (!combo || !cache)
        return;

    apply_flag_icons(combo, cache, source, iconSize);

    // Re-apply when the full image set arrives from the server.
    QObject::connect(cache, &ImageCache::allLoaded, context, [combo, cache, source, iconSize]() {
        apply_flag_icons(combo, cache, source, iconSize);
    });
}

void setup_currency_combo(QComboBox* combo,
                          QObject* owner,
                          ClientManager* client_manager,
                          ImageCache* image_cache,
                          std::function<QString()> fallback_selection,
                          QSize iconSize) {
    if (!combo || !owner || !client_manager || !client_manager->isConnected())
        return;

    const QString watcher_name = combo->objectName() + "CurrencyFetchWatcher";
    if (owner->findChild<QFutureWatcherBase*>(watcher_name))
        return;

    QPointer<QComboBox> comboPtr = combo;
    QPointer<QObject> ownerPtr = owner;
    auto future =
        QtConcurrent::run([client_manager]() { return fetch_currency_codes(client_manager); });

    auto* watcher = new QFutureWatcher<std::vector<std::string>>(owner);
    watcher->setObjectName(watcher_name);
    QObject::connect(watcher,
                     &QFutureWatcher<std::vector<std::string>>::finished,
                     owner,
                     [comboPtr, ownerPtr, watcher, image_cache, fallback_selection, iconSize]() {
                         auto codes = watcher->result();
                         watcher->deleteLater();
                         if (!comboPtr || !ownerPtr)
                             return;

                         std::sort(codes.begin(), codes.end());
                         const auto current = comboPtr->currentText();
                         // The initial async population races the dialog's own
                         // setEntity()-driven UI population: without blocking
                         // signals here, clear()/addItem()/setCurrentText() fire
                         // currentIndexChanged after the dialog has already reset
                         // its dirty flag, falsely marking an untouched form as
                         // having unsaved changes.
                         const QSignalBlocker blocker(comboPtr);
                         comboPtr->clear();
                         for (const auto& code : codes)
                             comboPtr->addItem(QString::fromStdString(code));

                         const QString to_select =
                             !current.isEmpty() ?
                                 current :
                                 (fallback_selection ? fallback_selection() : QString());
                         if (!to_select.isEmpty())
                             comboPtr->setCurrentText(to_select);

                         // setup_flag_combo, not apply_flag_icons: the image cache may
                         // still be loading when the combo first populates (icons would
                         // resolve empty and never be revisited), so this also
                         // reconnects on ImageCache::allLoaded to re-apply once the
                         // full set has downloaded.
                         setup_flag_combo(
                             ownerPtr, comboPtr, image_cache, FlagSource::Currency, iconSize);
                     });
    watcher->setFuture(future);
}

}
