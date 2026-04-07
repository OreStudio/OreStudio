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
#include "ores.qt/UiPersistence.hpp"

#include <QSettings>
#include <QWidget>
#include <QSplitter>
#include <QHeaderView>

namespace ores::qt {

namespace {

QSettings openSettings() {
    return QSettings("OreStudio", "OreStudio");
}

}

void UiPersistence::saveSize(const QString& group, const QWidget* w) {
    auto s = openSettings();
    s.beginGroup(group);
    s.setValue("windowSize", w->size());
    s.endGroup();
}

QSize UiPersistence::restoreSize(const QString& group, const QSize& defaultSize) {
    auto s = openSettings();
    s.beginGroup(group);
    const QSize result = s.value("windowSize", defaultSize).toSize();
    s.endGroup();
    return result.isValid() ? result : defaultSize;
}

void UiPersistence::saveSplitter(const QString& group, const QSplitter* spl) {
    auto s = openSettings();
    s.beginGroup(group);
    s.setValue("splitterState", spl->saveState());
    s.endGroup();
}

void UiPersistence::restoreSplitter(const QString& group, QSplitter* spl) {
    auto s = openSettings();
    s.beginGroup(group);
    if (s.contains("splitterState"))
        spl->restoreState(s.value("splitterState").toByteArray());
    s.endGroup();
}

void UiPersistence::saveHeader(const QString& group, const QHeaderView* h,
                                int version) {
    auto s = openSettings();
    s.beginGroup(group);
    s.setValue("headerState", h->saveState());
    s.setValue("settingsVersion", version);
    s.endGroup();
}

bool UiPersistence::restoreHeader(const QString& group, QHeaderView* h,
                                   int version) {
    auto s = openSettings();
    s.beginGroup(group);

    const int saved = s.value("settingsVersion", 0).toInt();
    if (saved == version && s.contains("headerState")) {
        h->restoreState(s.value("headerState").toByteArray());
        s.endGroup();
        return true;
    }

    // Version mismatch — discard stale state so next save is clean
    s.remove("headerState");
    s.setValue("settingsVersion", version);
    s.endGroup();
    return false;
}

}
