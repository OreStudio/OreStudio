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
#ifndef ORES_QT_ENTITY_DETAIL_DIALOG_HPP
#define ORES_QT_ENTITY_DETAIL_DIALOG_HPP

#include <QTableWidget>
#include <QToolBar>
#include <QTreeWidget>
#include <memory>
#include <unordered_map>
#include "ores.qt/ClientManager.hpp"
#include "ores.qt/DetailDialogBase.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/EntityDetailOperations.hpp"
#include "ores.logging/make_logger.hpp"
#include "ores.refdata/domain/party_id_scheme.hpp"

namespace ores::qt {
class ChangeReasonCache;
}

namespace Ui {
class EntityDetailDialog;
}

namespace ores::qt {

/**
 * @brief Shared detail dialog for viewing and editing party and counterparty
 * records.
 *
 * This dialog is parameterised via an entity_detail_operations implementation
 * to handle the protocol-level differences between parties and counterparties.
 * All UI code (tabs, sub-tables, hierarchy, metadata) is shared.
 */
class EntityDetailDialog final : public DetailDialogBase {
    Q_OBJECT

private:
    inline static std::string_view logger_name =
        "ores.qt.entity_detail_dialog";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit EntityDetailDialog(
        std::shared_ptr<entity_detail_operations> ops,
        QWidget* parent = nullptr);
    ~EntityDetailDialog() override;

    void setClientManager(ClientManager* clientManager);
    void setImageCache(ImageCache* imageCache);
    void setChangeReasonCache(ChangeReasonCache* changeReasonCache);
    void setUsername(const std::string& username);
    void setEntityData(const entity_data& data);
    void setCreateMode(bool createMode);
    void setReadOnly(bool readOnly);

signals:
    void entitySaved(const QString& code);
    void entityDeleted(const QString& code);

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

protected:
    QTabWidget* tabWidget() const override;
    QWidget* provenanceTab() const override;
    ProvenanceWidget* provenanceWidget() const override;

    bool hasUnsavedChanges() const override { return hasChanges_; }

private:
    void setupUi();
    void setupConnections();
    void setupIdentifierTable();
    void setupContactTable();
    void setupHierarchyTree();
    void populateLookups();
    void loadIdentifiers();
    void loadContacts();
    void loadAllEntities();
    void loadIdSchemes();
    void loadCountryImageMap();
    void populateParentCombo();
    void populateIdentifierTable();
    void populateContactTable();
    void buildHierarchyTree();
    void updateUiFromEntity();
    void updateEntityFromUi();
    void updateSaveButtonState();
    bool validateInput();

    std::shared_ptr<entity_detail_operations> ops_;
    Ui::EntityDetailDialog* ui_;
    ClientManager* clientManager_;
    ImageCache* imageCache_;
    ChangeReasonCache* changeReasonCache_;
    std::string username_;
    entity_data entity_;
    bool createMode_{true};
    bool readOnly_{false};
    bool hasChanges_{false};

    // All entities for parent combo + hierarchy
    std::vector<parent_entity_entry> allEntities_;

    // Identifier sub-table
    QTableWidget* identifierTable_;
    QToolBar* identifierToolbar_;
    std::vector<identifier_entry> identifiers_;

    // Contact sub-table
    QTableWidget* contactTable_;
    QToolBar* contactToolbar_;
    std::vector<contact_entry> contacts_;

    // Identifier schemes
    std::vector<refdata::domain::party_id_scheme> idSchemes_;

    // Country code -> image_id mapping for flag icons
    std::unordered_map<std::string, std::string> countryImageMap_;

    // Hierarchy
    QTreeWidget* hierarchyTree_;
};

}

#endif
