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
#ifndef ORES_QT_LEI_ENTITY_PICKER_HPP
#define ORES_QT_LEI_ENTITY_PICKER_HPP

#include <QWidget>
#include <QLineEdit>
#include <QComboBox>
#include <QTableView>
#include <QLabel>
#include <QStandardItemModel>
#include <QSortFilterProxyModel>
#include "ores.qt/ClientManager.hpp"
#include "ores.logging/make_logger.hpp"

namespace ores::qt {

/**
 * @brief Reusable widget for searching and selecting an LEI entity.
 *
 * Loads LEI entity summaries from the DQ staging data via the binary
 * protocol and presents them in a filterable table. Users can search
 * by entity legal name and select an entity to use in other screens.
 */
class LeiEntityPicker final : public QWidget {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.lei_entity_picker";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit LeiEntityPicker(ClientManager* clientManager,
        QWidget* parent = nullptr);

    /**
     * @brief Get the LEI of the currently selected entity.
     * @return LEI string, or empty if no selection.
     */
    QString selectedLei() const;

    /**
     * @brief Get the entity legal name of the currently selected entity.
     * @return Entity legal name, or empty if no selection.
     */
    QString selectedName() const;

    /**
     * @brief Check whether an entity is currently selected.
     */
    bool hasSelection() const;

    /**
     * @brief Trigger an asynchronous load of LEI entities from the server.
     */
    void load();

signals:
    /**
     * @brief Emitted when the user selects an entity.
     * @param lei The LEI of the selected entity
     * @param name The entity legal name of the selected entity
     */
    void entitySelected(const QString& lei, const QString& name);

    /**
     * @brief Emitted when the selection is cleared.
     */
    void selectionCleared();

    /**
     * @brief Emitted when entities are successfully loaded from the server.
     * @param entityCount Number of entities loaded
     */
    void loadCompleted(int entityCount);

    /**
     * @brief Emitted when loading entities fails.
     * @param errorMessage Description of the failure
     */
    void loadFailed(const QString& errorMessage);

private slots:
    void onSearchTextChanged(const QString& text);
    void onCountryFilterChanged(int index);
    void onSelectionChanged();

private:
    void setupUI();
    void applyFilters();
    void loadEntitiesForCountry(const QString& country);

    ClientManager* clientManager_;
    QLineEdit* searchEdit_;
    QComboBox* countryFilter_;
    QTableView* tableView_;
    QStandardItemModel* model_;
    QSortFilterProxyModel* proxyModel_;
    QLabel* statusLabel_;
    QString selectedLei_;
    QString selectedName_;
    bool countriesLoaded_ = false;
};

}

#endif
