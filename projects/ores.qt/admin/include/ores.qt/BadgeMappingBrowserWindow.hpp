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
#ifndef ORES_QT_BADGE_MAPPING_BROWSER_WINDOW_HPP
#define ORES_QT_BADGE_MAPPING_BROWSER_WINDOW_HPP

#include "ores.logging/make_logger.hpp"
#include <QComboBox>
#include <QLabel>
#include <QTableWidget>
#include <QVBoxLayout>
#include <QWidget>

namespace ores::qt {

class BadgeCache;

/**
 * @brief Read-only browser for badge_mapping: shows every
 * (code_domain, entity_code) -> badge_definition linkage, grouped by
 * code_domain, with each badge rendered as an actual colour pill.
 *
 * badge_mapping has no CRUD UI (populated via seed scripts only), so
 * unlike every other admin window this one is a plain QWidget reading
 * data already held by BadgeCache -- no network fetch, no controller,
 * no EntityController machinery. Its purpose is purely to make badge
 * source self-use, adoption coverage, and colour-semantic misuse
 * reviewable in the UI instead of grep-only.
 */
class BadgeMappingBrowserWindow final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.badge_mapping_browser_window";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit BadgeMappingBrowserWindow(BadgeCache* badgeCache, QWidget* parent = nullptr);

public slots:
    /**
     * @brief Repopulates the domain picker and current table from
     * BadgeCache's currently-loaded data. Safe to call before the cache
     * has finished loading (renders nothing until it has).
     */
    void reload();

private slots:
    void onDomainChanged(int index);

private:
    void setupUi();
    void populateDomainPicker();
    void populateTable(const QString& codeDomainCode);

    BadgeCache* badgeCache_;
    QComboBox* domainPicker_;
    QTableWidget* table_;
    QLabel* emptyLabel_;
};

}

#endif
