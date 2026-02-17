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
#ifndef ORES_QT_COUNTERPARTY_DETAIL_DIALOG_HPP
#define ORES_QT_COUNTERPARTY_DETAIL_DIALOG_HPP

#include <QTableWidget>
#include <QToolBar>
#include <QTreeWidget>
#include <unordered_map>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/counterparty.hpp"
#include "ores.refdata/domain/counterparty_identifier.hpp"
#include "ores.refdata/domain/counterparty_contact_information.hpp"
#include "ores.refdata/domain/party_id_scheme.hpp"

namespace ores::qt {
class ChangeReasonCache;
}

namespace Ui {
class CounterpartyDetailDialog;
}

namespace ores::qt {

/**
 * @brief Detail dialog for viewing and editing counterparty records.
 *
 * This dialog allows viewing, creating, and editing counterparties.
 * It supports both create mode (for new records) and edit mode (for
 * existing records). Includes tabbed view with sub-tables for
 * identifiers and contacts, and a hierarchy tree.
 */
class CounterpartyDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.counterparty_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit CounterpartyDetailDialog(QWidget* parent = nullptr);
    ~CounterpartyDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setImageCache(ImageCache* imageCache);
    void setChangeReasonCache(ChangeReasonCache* changeReasonCache);
    void setUsername(const std::string& username);
    void setCounterparty(const refdata::domain::counterparty& counterparty);
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly);

signals:
    void counterpartySaved(const QString& code);
    void counterpartyDeleted(const QString& code);

private slots:
    void onSaveClicked();
    void onDeleteClicked();
    void onCodeChanged(const QString& text);
    void onFieldChanged();
    void onAddIdentifier();
    void onDeleteIdentifier();
    void onAddContact();
    void onDeleteContact();
    void onContactDoubleClicked(int row, int column);

private:
    void setupUi();
    void setupConnections();
    void setupIdentifierTable();
    void setupContactTable();
    void setupHierarchyTree();
    void populateLookups();
    void loadIdentifiers();
    void loadContacts();
    void loadAllCounterparties();
    void loadIdSchemes();
    void loadCountryImageMap();
    void populateParentCombo();
    void populateIdentifierTable();
    void populateContactTable();
    void buildHierarchyTree();
    void updateUiFromCounterparty();
    void updateCounterpartyFromUi();
    void updateSaveButtonState();
    bool validateInput();

    Ui::CounterpartyDetailDialog* ui_;
    ClientManager* clientManager_;
    ImageCache* imageCache_;
    ChangeReasonCache* changeReasonCache_;
    std::string username_;
    refdata::domain::counterparty counterparty_;
    bool createMode_{true};
    bool readOnly_{false};
    bool hasChanges_{false};

    // All counterparties for parent combo + hierarchy
    std::vector<refdata::domain::counterparty> allCounterparties_;

    // Identifier sub-table
    QTableWidget* identifierTable_;
    QToolBar* identifierToolbar_;
    std::vector<refdata::domain::counterparty_identifier> identifiers_;

    // Contact sub-table
    QTableWidget* contactTable_;
    QToolBar* contactToolbar_;
    std::vector<refdata::domain::counterparty_contact_information> contacts_;

    // Identifier schemes
    std::vector<refdata::domain::party_id_scheme> idSchemes_;

    // Country code -> image_id mapping for flag icons
    std::unordered_map<std::string, std::string> countryImageMap_;

    // Hierarchy
    QTreeWidget* hierarchyTree_;
};

}

#endif
