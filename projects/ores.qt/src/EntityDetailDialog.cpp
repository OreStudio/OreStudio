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
#include "ores.qt/EntityDetailDialog.hpp"

#include <algorithm>
#include <QAction>
#include <QComboBox>
#include <QDialog>
#include <QDialogButtonBox>
#include <QFormLayout>
#include <QHeaderView>
#include <QLineEdit>
#include <QMessageBox>
#include <QVBoxLayout>
#include <QtConcurrent>
#include <QFutureWatcher>
#include <boost/uuid/uuid_io.hpp>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include "ui_EntityDetailDialog.h"
#include "ores.qt/FlagIconHelper.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata/messaging/party_id_scheme_protocol.hpp"
#include "ores.refdata/messaging/country_protocol.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.dq/domain/change_reason_constants.hpp"

namespace ores::qt {

using namespace ores::logging;

EntityDetailDialog::EntityDetailDialog(
    std::shared_ptr<entity_detail_operations> ops,
    QWidget* parent)
    : DetailDialogBase(parent),
      ops_(std::move(ops)),
      ui_(new Ui::EntityDetailDialog),
      clientManager_(nullptr),
      imageCache_(nullptr),
      changeReasonCache_(nullptr),
      identifierTable_(nullptr),
      identifierToolbar_(nullptr),
      contactTable_(nullptr),
      contactToolbar_(nullptr),
      hierarchyTree_(nullptr) {

    ui_->setupUi(this);
    setupUi();
    setupIdentifierTable();
    setupContactTable();
    setupHierarchyTree();
    setupConnections();
}

EntityDetailDialog::~EntityDetailDialog() {
    delete ui_;
}

void EntityDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    // Show/hide party_category based on entity type
    if (ops_->has_party_category()) {
        ui_->partyCategoryCombo->addItem(
            QString::fromUtf8(party_categories::operational));
        ui_->partyCategoryCombo->addItem(
            QString::fromUtf8(party_categories::system));
        ui_->partyCategoryCombo->addItem(
            QString::fromUtf8(party_categories::internal));
    } else {
        ui_->label_partyCategoryEdit->setVisible(false);
        ui_->partyCategoryCombo->setVisible(false);
    }
}

void EntityDetailDialog::setupIdentifierTable() {
    identifierToolbar_ = new QToolBar(this);
    identifierToolbar_->setIconSize(QSize(16, 16));

    auto* addAction = identifierToolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        "Add Identifier");
    auto* deleteAction = identifierToolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor),
        "Delete Identifier");

    connect(addAction, &QAction::triggered, this,
            &EntityDetailDialog::onAddIdentifier);
    connect(deleteAction, &QAction::triggered, this,
            &EntityDetailDialog::onDeleteIdentifier);

    identifierTable_ = new QTableWidget(this);
    identifierTable_->setColumnCount(3);
    identifierTable_->setHorizontalHeaderLabels({"Scheme", "Value", "Description"});
    identifierTable_->horizontalHeader()->setStretchLastSection(true);
    identifierTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    identifierTable_->setSelectionMode(QAbstractItemView::SingleSelection);
    identifierTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    identifierTable_->verticalHeader()->setVisible(false);

    ui_->identifiersLayout->addWidget(identifierToolbar_);
    ui_->identifiersLayout->addWidget(identifierTable_);
}

void EntityDetailDialog::setupContactTable() {
    contactToolbar_ = new QToolBar(this);
    contactToolbar_->setIconSize(QSize(16, 16));

    auto* addAction = contactToolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        "Add Contact");
    auto* deleteAction = contactToolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor),
        "Delete Contact");

    connect(addAction, &QAction::triggered, this,
            &EntityDetailDialog::onAddContact);
    connect(deleteAction, &QAction::triggered, this,
            &EntityDetailDialog::onDeleteContact);

    contactTable_ = new QTableWidget(this);
    contactTable_->setColumnCount(6);
    contactTable_->setHorizontalHeaderLabels(
        {"", "Country", "Type", "Street", "City", "Phone"});
    contactTable_->setColumnWidth(0, 28);
    contactTable_->horizontalHeader()->setStretchLastSection(true);
    contactTable_->setSelectionBehavior(QAbstractItemView::SelectRows);
    contactTable_->setSelectionMode(QAbstractItemView::SingleSelection);
    contactTable_->setEditTriggers(QAbstractItemView::NoEditTriggers);
    contactTable_->verticalHeader()->setVisible(false);

    ui_->contactsLayout->addWidget(contactToolbar_);
    ui_->contactsLayout->addWidget(contactTable_);

    connect(contactTable_, &QTableWidget::cellDoubleClicked, this,
            &EntityDetailDialog::onContactDoubleClicked);
}

void EntityDetailDialog::setupHierarchyTree() {
    hierarchyTree_ = new QTreeWidget(this);
    hierarchyTree_->setHeaderLabels({"Name", "Short Code", "Status"});
    hierarchyTree_->setColumnCount(3);
    hierarchyTree_->header()->setStretchLastSection(true);

    ui_->hierarchyLayout->addWidget(hierarchyTree_);
}

void EntityDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &EntityDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &EntityDetailDialog::onDeleteClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &EntityDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &EntityDetailDialog::onFieldChanged);
    connect(ui_->transliteratedNameEdit, &QLineEdit::textChanged, this,
            &EntityDetailDialog::onFieldChanged);
    connect(ui_->partyTypeCombo, &QComboBox::currentTextChanged, this,
            &EntityDetailDialog::onFieldChanged);
    connect(ui_->parentEntityCombo, &QComboBox::currentTextChanged, this,
            &EntityDetailDialog::onFieldChanged);
    connect(ui_->statusCombo, &QComboBox::currentTextChanged, this,
            &EntityDetailDialog::onFieldChanged);
    connect(ui_->businessCenterCombo, &QComboBox::currentTextChanged, this,
            &EntityDetailDialog::onFieldChanged);

    if (ops_->has_party_category()) {
        connect(ui_->partyCategoryCombo, &QComboBox::currentTextChanged, this,
                &EntityDetailDialog::onFieldChanged);
    }
}

void EntityDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateLookups();
}

void EntityDetailDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
    if (imageCache_) {
        connect(imageCache_, &ImageCache::allLoaded, this, [this]() {
            set_combo_flag_icons(ui_->businessCenterCombo,
                [this](const std::string& code) {
                    return imageCache_->getBusinessCentreFlagIcon(code);
                });
        });
    }
}

void EntityDetailDialog::setChangeReasonCache(ChangeReasonCache* changeReasonCache) {
    changeReasonCache_ = changeReasonCache;
}

void EntityDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void EntityDetailDialog::populateLookups() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        return;
    }

    QPointer<EntityDetailDialog> self = this;
    auto* cm = clientManager_;

    auto task = [cm]() -> lookup_result {
        return fetch_party_lookups(cm);
    };

    auto* watcher = new QFutureWatcher<lookup_result>(self);
    connect(watcher, &QFutureWatcher<lookup_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        self->ui_->partyTypeCombo->clear();
        for (const auto& code : result.type_codes) {
            self->ui_->partyTypeCombo->addItem(
                QString::fromStdString(code));
        }

        self->ui_->statusCombo->clear();
        for (const auto& code : result.status_codes) {
            self->ui_->statusCombo->addItem(
                QString::fromStdString(code));
        }

        self->ui_->businessCenterCombo->clear();
        for (const auto& code : result.business_centre_codes) {
            self->ui_->businessCenterCombo->addItem(
                QString::fromStdString(code));
        }

        if (self->imageCache_) {
            set_combo_flag_icons(self->ui_->businessCenterCombo,
                [&self](const std::string& code) {
                    return self->imageCache_->getBusinessCentreFlagIcon(code);
                });
        }

        self->updateUiFromEntity();

        // Also load all entities for parent combo + hierarchy
        self->loadAllEntities();
        self->loadIdSchemes();
        self->loadCountryImageMap();
    });

    QFuture<lookup_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void EntityDetailDialog::loadAllEntities() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        return;
    }

    QPointer<EntityDetailDialog> self = this;
    auto* cm = clientManager_;
    auto ops = ops_;

    auto task = [cm, ops]() -> load_all_entities_result {
        return ops->load_all_entities(cm);
    };

    auto* watcher = new QFutureWatcher<load_all_entities_result>(self);
    connect(watcher, &QFutureWatcher<load_all_entities_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self || !result.success) return;

        self->allEntities_ = std::move(result.entities);
        self->populateParentCombo();
        self->buildHierarchyTree();
    });

    QFuture<load_all_entities_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void EntityDetailDialog::loadIdSchemes() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<EntityDetailDialog> self = this;
    auto* cm = clientManager_;

    struct SchemesResult {
        std::vector<refdata::domain::party_id_scheme> schemes;
        bool success;
    };

    auto task = [cm]() -> SchemesResult {
        refdata::messaging::get_party_id_schemes_request request;
        auto response = cm->process_authenticated_request(std::move(request));
        if (!response) return {{}, false};
        return {std::move(response->schemes), true};
    };

    auto* watcher = new QFutureWatcher<SchemesResult>(self);
    connect(watcher, &QFutureWatcher<SchemesResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self || !result.success) return;

        self->idSchemes_ = std::move(result.schemes);

        // Sort by display_order
        std::sort(self->idSchemes_.begin(), self->idSchemes_.end(),
            [](const auto& a, const auto& b) {
                return a.display_order < b.display_order;
            });
    });

    QFuture<SchemesResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void EntityDetailDialog::loadCountryImageMap() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<EntityDetailDialog> self = this;
    auto* cm = clientManager_;

    using MapType = std::unordered_map<std::string, std::string>;

    auto task = [cm]() -> MapType {
        refdata::messaging::get_countries_request request;
        request.limit = 1000;
        auto response = cm->process_authenticated_request(std::move(request));
        if (!response) return {};

        MapType mapping;
        for (const auto& country : response->countries) {
            if (country.image_id) {
                mapping.emplace(country.alpha2_code,
                    boost::uuids::to_string(*country.image_id));
            }
        }
        return mapping;
    };

    auto* watcher = new QFutureWatcher<MapType>(self);
    connect(watcher, &QFutureWatcher<MapType>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        self->countryImageMap_ = std::move(result);
        // Re-populate contacts table if already loaded
        if (!self->contacts_.empty()) {
            self->populateContactTable();
        }
    });

    QFuture<MapType> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void EntityDetailDialog::populateParentCombo() {
    ui_->parentEntityCombo->clear();
    ui_->parentEntityCombo->addItem("(None)", QVariant());

    for (const auto& entry : allEntities_) {
        // Don't allow setting self as parent
        if (entry.id == entity_.id) continue;

        QString label = QString::fromStdString(entry.short_code) + " - " +
                        QString::fromStdString(entry.full_name);
        ui_->parentEntityCombo->addItem(
            label, QString::fromStdString(boost::uuids::to_string(entry.id)));
    }

    // Set current value
    if (entity_.parent_id) {
        const auto parent_id_str = boost::uuids::to_string(*entity_.parent_id);
        for (int i = 0; i < ui_->parentEntityCombo->count(); ++i) {
            if (ui_->parentEntityCombo->itemData(i).toString().toStdString() == parent_id_str) {
                ui_->parentEntityCombo->setCurrentIndex(i);
                break;
            }
        }
    } else {
        ui_->parentEntityCombo->setCurrentIndex(0);
    }
}

void EntityDetailDialog::buildHierarchyTree() {
    hierarchyTree_->clear();

    if (allEntities_.empty()) return;

    // Build a map of id -> entry
    std::unordered_map<std::string, const parent_entity_entry*> byId;
    for (const auto& entry : allEntities_) {
        byId[boost::uuids::to_string(entry.id)] = &entry;
    }

    // Build a map of parent_id -> children
    std::unordered_map<std::string, std::vector<const parent_entity_entry*>> children;
    std::vector<const parent_entity_entry*> roots;

    for (const auto& entry : allEntities_) {
        if (entry.parent_id) {
            children[boost::uuids::to_string(*entry.parent_id)].push_back(&entry);
        } else {
            roots.push_back(&entry);
        }
    }

    const auto current_id_str = boost::uuids::to_string(entity_.id);

    // Recursive tree builder
    std::function<QTreeWidgetItem*(const parent_entity_entry*)> buildItem;
    buildItem = [&](const parent_entity_entry* entry) -> QTreeWidgetItem* {
        auto* item = new QTreeWidgetItem();
        const auto id_str = boost::uuids::to_string(entry->id);
        const bool isCurrent = (id_str == current_id_str);

        QString name = QString::fromStdString(entry->full_name);
        if (isCurrent) {
            name += " [THIS]";
        }
        item->setText(0, name);
        item->setText(1, QString::fromStdString(entry->short_code));
        item->setText(2, QString::fromStdString(entry->status));

        if (isCurrent) {
            QFont font = item->font(0);
            font.setBold(true);
            item->setFont(0, font);
            item->setFont(1, font);
            item->setFont(2, font);
        }

        auto it = children.find(id_str);
        if (it != children.end()) {
            for (const auto* child : it->second) {
                item->addChild(buildItem(child));
            }
        }

        return item;
    };

    for (const auto* root : roots) {
        hierarchyTree_->addTopLevelItem(buildItem(root));
    }

    hierarchyTree_->expandAll();
    hierarchyTree_->resizeColumnToContents(0);
    hierarchyTree_->resizeColumnToContents(1);
}

void EntityDetailDialog::setEntityData(const entity_data& data) {
    entity_ = data;
    updateUiFromEntity();
    loadIdentifiers();
    loadContacts();
}

void EntityDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode) {
        // Hide metadata tab - no data exists yet
        ui_->tabWidget->removeTab(ui_->tabWidget->indexOf(ui_->metadataTab));

        // Disable sub-entity toolbars until entity is saved
        if (identifierToolbar_) identifierToolbar_->setEnabled(false);
        if (contactToolbar_) contactToolbar_->setEnabled(false);
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void EntityDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->transliteratedNameEdit->setReadOnly(readOnly);
    ui_->partyTypeCombo->setEnabled(!readOnly);
    ui_->parentEntityCombo->setEnabled(!readOnly);
    ui_->statusCombo->setEnabled(!readOnly);
    ui_->businessCenterCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);

    if (ops_->has_party_category()) {
        ui_->partyCategoryCombo->setEnabled(!readOnly);
    }

    if (identifierToolbar_) identifierToolbar_->setVisible(!readOnly);
    if (contactToolbar_) contactToolbar_->setVisible(!readOnly);
}

void EntityDetailDialog::updateUiFromEntity() {
    ui_->codeEdit->setText(QString::fromStdString(entity_.short_code));
    ui_->nameEdit->setText(QString::fromStdString(entity_.full_name));
    ui_->transliteratedNameEdit->setText(
        QString::fromStdString(entity_.transliterated_name.value_or("")));
    ui_->partyTypeCombo->setCurrentText(QString::fromStdString(entity_.party_type));
    ui_->statusCombo->setCurrentText(QString::fromStdString(entity_.status));
    ui_->businessCenterCombo->setCurrentText(QString::fromStdString(entity_.business_center_code));

    if (ops_->has_party_category() && entity_.party_category) {
        ui_->partyCategoryCombo->setCurrentText(
            QString::fromStdString(*entity_.party_category));
    }

    ui_->versionEdit->setText(QString::number(entity_.version));
    ui_->modifiedByEdit->setText(QString::fromStdString(entity_.modified_by));
    ui_->changeReasonEdit->setText(QString::fromStdString(entity_.change_reason_code));
    ui_->recordedAtEdit->setText(relative_time_helper::format(entity_.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(entity_.change_commentary));
}

void EntityDetailDialog::updateEntityFromUi() {
    if (createMode_) {
        entity_.short_code = ui_->codeEdit->text().trimmed().toStdString();
    }
    entity_.full_name = ui_->nameEdit->text().trimmed().toStdString();
    const auto tlit = ui_->transliteratedNameEdit->text().trimmed().toStdString();
    entity_.transliterated_name = tlit.empty() ? std::nullopt : std::optional<std::string>(tlit);
    entity_.party_type = ui_->partyTypeCombo->currentText().trimmed().toStdString();
    entity_.status = ui_->statusCombo->currentText().trimmed().toStdString();
    const auto bcc = ui_->businessCenterCombo->currentText().trimmed().toStdString();
    entity_.business_center_code = bcc.empty() ? std::string("WRLD") : bcc;
    entity_.modified_by = username_;
    entity_.performed_by = username_;

    if (ops_->has_party_category()) {
        entity_.party_category = ui_->partyCategoryCombo->currentText().trimmed().toStdString();
    }

    // Parent entity
    int parentIndex = ui_->parentEntityCombo->currentIndex();
    QVariant parentData = ui_->parentEntityCombo->itemData(parentIndex);
    if (parentData.isValid() && !parentData.toString().isEmpty()) {
        auto parent_id_str = parentData.toString().toStdString();
        boost::uuids::string_generator gen;
        entity_.parent_id = gen(parent_id_str);
    } else {
        entity_.parent_id = std::nullopt;
    }
}

void EntityDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void EntityDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void EntityDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool EntityDetailDialog::validateInput() {
    const QString short_code_val = ui_->codeEdit->text().trimmed();
    const QString full_name_val = ui_->nameEdit->text().trimmed();

    return !short_code_val.isEmpty() && !full_name_val.isEmpty();
}

// ============================================================================
// Identifier sub-table
// ============================================================================

void EntityDetailDialog::loadIdentifiers() {
    if (!clientManager_ || !clientManager_->isConnected()) return;
    if (entity_.id.is_nil()) return;

    QPointer<EntityDetailDialog> self = this;
    auto* cm = clientManager_;
    auto entity_id = entity_.id;
    auto ops = ops_;

    auto task = [cm, entity_id, ops]() -> load_identifiers_result {
        return ops->load_identifiers(cm, entity_id);
    };

    auto* watcher = new QFutureWatcher<load_identifiers_result>(self);
    connect(watcher, &QFutureWatcher<load_identifiers_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self || !result.success) return;

        self->identifiers_ = std::move(result.identifiers);
        self->populateIdentifierTable();
    });

    QFuture<load_identifiers_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void EntityDetailDialog::populateIdentifierTable() {
    identifierTable_->setRowCount(0);
    identifierTable_->setRowCount(static_cast<int>(identifiers_.size()));

    for (int i = 0; i < static_cast<int>(identifiers_.size()); ++i) {
        const auto& ident = identifiers_[static_cast<std::size_t>(i)];
        identifierTable_->setItem(i, 0, new QTableWidgetItem(QString::fromStdString(ident.id_scheme)));
        identifierTable_->setItem(i, 1, new QTableWidgetItem(QString::fromStdString(ident.id_value)));
        identifierTable_->setItem(i, 2, new QTableWidgetItem(QString::fromStdString(ident.description)));
    }

    identifierTable_->resizeColumnsToContents();
}

void EntityDetailDialog::onAddIdentifier() {
    if (!clientManager_ || !clientManager_->isConnected() || readOnly_) return;

    QDialog dialog(this);
    dialog.setWindowTitle("Add Identifier");
    dialog.setMinimumWidth(400);

    auto* layout = new QFormLayout(&dialog);

    auto* schemeCombo = new QComboBox(&dialog);
    for (const auto& scheme : idSchemes_) {
        QString label = QString::fromStdString(scheme.code) + " - " +
                        QString::fromStdString(scheme.name);
        schemeCombo->addItem(label, QString::fromStdString(scheme.code));
    }
    layout->addRow("Scheme:", schemeCombo);

    auto* valueEdit = new QLineEdit(&dialog);
    valueEdit->setPlaceholderText("Identifier value");
    layout->addRow("Value:", valueEdit);

    auto* descEdit = new QLineEdit(&dialog);
    descEdit->setPlaceholderText("Optional description");
    layout->addRow("Description:", descEdit);

    auto* buttonBox = new QDialogButtonBox(
        QDialogButtonBox::Ok | QDialogButtonBox::Cancel, &dialog);
    layout->addRow(buttonBox);

    connect(buttonBox, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);
    connect(buttonBox, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);

    if (dialog.exec() != QDialog::Accepted) return;

    if (schemeCombo->currentIndex() < 0 || valueEdit->text().trimmed().isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Scheme and Value are required.");
        return;
    }

    QPointer<EntityDetailDialog> self = this;
    auto* cm = clientManager_;
    auto ops = ops_;

    identifier_entry newIdent;
    boost::uuids::random_generator uuid_gen;
    newIdent.id = uuid_gen();
    newIdent.owner_id = entity_.id;
    newIdent.id_scheme = schemeCombo->currentData().toString().toStdString();
    newIdent.id_value = valueEdit->text().trimmed().toStdString();
    newIdent.description = descEdit->text().trimmed().toStdString();
    newIdent.modified_by = username_;
    newIdent.performed_by = username_;

    auto task = [cm, ops, newIdent]() -> operation_result {
        return ops->save_identifier(cm, newIdent);
    };

    auto* watcher = new QFutureWatcher<operation_result>(self);
    connect(watcher, &QFutureWatcher<operation_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            self->loadIdentifiers();
        } else {
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(result.message));
        }
    });

    QFuture<operation_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void EntityDetailDialog::onDeleteIdentifier() {
    if (!clientManager_ || !clientManager_->isConnected() || readOnly_) return;

    int row = identifierTable_->currentRow();
    if (row < 0 || row >= static_cast<int>(identifiers_.size())) {
        MessageBoxHelper::warning(this, "No Selection",
            "Please select an identifier to delete.");
        return;
    }

    const auto& ident = identifiers_[static_cast<std::size_t>(row)];
    auto reply = MessageBoxHelper::question(this, "Delete Identifier",
        QString("Delete identifier '%1: %2'?")
            .arg(QString::fromStdString(ident.id_scheme))
            .arg(QString::fromStdString(ident.id_value)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<EntityDetailDialog> self = this;
    auto* cm = clientManager_;
    auto ops = ops_;
    auto identId = ident.id;

    auto task = [cm, ops, identId]() -> operation_result {
        return ops->delete_identifier(cm, identId);
    };

    auto* watcher = new QFutureWatcher<operation_result>(self);
    connect(watcher, &QFutureWatcher<operation_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            self->loadIdentifiers();
        } else {
            MessageBoxHelper::critical(self, "Delete Failed",
                QString::fromStdString(result.message));
        }
    });

    QFuture<operation_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

// ============================================================================
// Contact sub-table
// ============================================================================

void EntityDetailDialog::loadContacts() {
    if (!clientManager_ || !clientManager_->isConnected()) return;
    if (entity_.id.is_nil()) return;

    QPointer<EntityDetailDialog> self = this;
    auto* cm = clientManager_;
    auto entity_id = entity_.id;
    auto ops = ops_;

    auto task = [cm, entity_id, ops]() -> load_contacts_result {
        return ops->load_contacts(cm, entity_id);
    };

    auto* watcher = new QFutureWatcher<load_contacts_result>(self);
    connect(watcher, &QFutureWatcher<load_contacts_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self || !result.success) return;

        self->contacts_ = std::move(result.contacts);
        self->populateContactTable();
    });

    QFuture<load_contacts_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void EntityDetailDialog::populateContactTable() {
    contactTable_->setRowCount(0);
    contactTable_->setRowCount(static_cast<int>(contacts_.size()));

    for (int i = 0; i < static_cast<int>(contacts_.size()); ++i) {
        const auto& contact = contacts_[static_cast<std::size_t>(i)];

        // Flag icon column
        auto* flagItem = new QTableWidgetItem();
        if (imageCache_) {
            auto it = countryImageMap_.find(contact.country_code);
            if (it != countryImageMap_.end() && !it->second.empty()) {
                flagItem->setIcon(imageCache_->getIcon(it->second));
            } else {
                flagItem->setIcon(imageCache_->getNoFlagIcon());
            }
        }
        contactTable_->setItem(i, 0, flagItem);

        contactTable_->setItem(i, 1, new QTableWidgetItem(QString::fromStdString(contact.country_code)));
        contactTable_->setItem(i, 2, new QTableWidgetItem(QString::fromStdString(contact.contact_type)));
        contactTable_->setItem(i, 3, new QTableWidgetItem(QString::fromStdString(contact.street_line_1)));
        contactTable_->setItem(i, 4, new QTableWidgetItem(QString::fromStdString(contact.city)));
        contactTable_->setItem(i, 5, new QTableWidgetItem(QString::fromStdString(contact.phone)));
    }

    contactTable_->resizeColumnsToContents();
}

void EntityDetailDialog::onAddContact() {
    if (!clientManager_ || !clientManager_->isConnected() || readOnly_) return;

    QDialog dialog(this);
    dialog.setWindowTitle("Add Contact Information");
    dialog.setMinimumWidth(450);

    auto* layout = new QFormLayout(&dialog);

    auto* typeEdit = new QLineEdit(&dialog);
    typeEdit->setPlaceholderText("e.g. Legal, Operations, Settlement");
    layout->addRow("Type:", typeEdit);

    auto* streetLine1Edit = new QLineEdit(&dialog);
    layout->addRow("Street Line 1:", streetLine1Edit);

    auto* streetLine2Edit = new QLineEdit(&dialog);
    layout->addRow("Street Line 2:", streetLine2Edit);

    auto* cityEdit = new QLineEdit(&dialog);
    layout->addRow("City:", cityEdit);

    auto* stateEdit = new QLineEdit(&dialog);
    layout->addRow("State:", stateEdit);

    auto* countryEdit = new QLineEdit(&dialog);
    countryEdit->setPlaceholderText("ISO 3166-1 alpha-2");
    layout->addRow("Country Code:", countryEdit);

    auto* postalEdit = new QLineEdit(&dialog);
    layout->addRow("Postal Code:", postalEdit);

    auto* phoneEdit = new QLineEdit(&dialog);
    layout->addRow("Phone:", phoneEdit);

    auto* emailEdit = new QLineEdit(&dialog);
    layout->addRow("Email:", emailEdit);

    auto* webEdit = new QLineEdit(&dialog);
    layout->addRow("Web Page:", webEdit);

    auto* buttonBox = new QDialogButtonBox(
        QDialogButtonBox::Ok | QDialogButtonBox::Cancel, &dialog);
    layout->addRow(buttonBox);

    connect(buttonBox, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);
    connect(buttonBox, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);

    if (dialog.exec() != QDialog::Accepted) return;

    if (typeEdit->text().trimmed().isEmpty()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Contact Type is required.");
        return;
    }

    QPointer<EntityDetailDialog> self = this;
    auto* cm = clientManager_;
    auto ops = ops_;

    contact_entry newContact;
    boost::uuids::random_generator uuid_gen;
    newContact.id = uuid_gen();
    newContact.owner_id = entity_.id;
    newContact.contact_type = typeEdit->text().trimmed().toStdString();
    newContact.street_line_1 = streetLine1Edit->text().trimmed().toStdString();
    newContact.street_line_2 = streetLine2Edit->text().trimmed().toStdString();
    newContact.city = cityEdit->text().trimmed().toStdString();
    newContact.state = stateEdit->text().trimmed().toStdString();
    newContact.country_code = countryEdit->text().trimmed().toStdString();
    newContact.postal_code = postalEdit->text().trimmed().toStdString();
    newContact.phone = phoneEdit->text().trimmed().toStdString();
    newContact.email = emailEdit->text().trimmed().toStdString();
    newContact.web_page = webEdit->text().trimmed().toStdString();
    newContact.modified_by = username_;
    newContact.performed_by = username_;

    auto task = [cm, ops, newContact]() -> operation_result {
        return ops->save_contact(cm, newContact);
    };

    auto* watcher = new QFutureWatcher<operation_result>(self);
    connect(watcher, &QFutureWatcher<operation_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            self->loadContacts();
        } else {
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(result.message));
        }
    });

    QFuture<operation_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void EntityDetailDialog::onDeleteContact() {
    if (!clientManager_ || !clientManager_->isConnected() || readOnly_) return;

    int row = contactTable_->currentRow();
    if (row < 0 || row >= static_cast<int>(contacts_.size())) {
        MessageBoxHelper::warning(this, "No Selection",
            "Please select a contact to delete.");
        return;
    }

    const auto& contact = contacts_[static_cast<std::size_t>(row)];
    auto reply = MessageBoxHelper::question(this, "Delete Contact",
        QString("Delete contact '%1'?")
            .arg(QString::fromStdString(contact.contact_type)),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) return;

    QPointer<EntityDetailDialog> self = this;
    auto* cm = clientManager_;
    auto ops = ops_;
    auto contactId = contact.id;

    auto task = [cm, ops, contactId]() -> operation_result {
        return ops->delete_contact(cm, contactId);
    };

    auto* watcher = new QFutureWatcher<operation_result>(self);
    connect(watcher, &QFutureWatcher<operation_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            self->loadContacts();
        } else {
            MessageBoxHelper::critical(self, "Delete Failed",
                QString::fromStdString(result.message));
        }
    });

    QFuture<operation_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void EntityDetailDialog::onContactDoubleClicked(int row, int /* column */) {
    if (row < 0 || row >= static_cast<int>(contacts_.size())) return;

    const auto& contact = contacts_[static_cast<std::size_t>(row)];
    const bool editable = !readOnly_;

    QDialog dialog(this);
    dialog.setWindowTitle("Contact Information Details");
    dialog.setMinimumWidth(500);

    auto* layout = new QFormLayout(&dialog);

    auto* typeEdit = new QLineEdit(QString::fromStdString(contact.contact_type), &dialog);
    typeEdit->setReadOnly(!editable);
    layout->addRow("Type:", typeEdit);

    auto* streetLine1Edit = new QLineEdit(QString::fromStdString(contact.street_line_1), &dialog);
    streetLine1Edit->setReadOnly(!editable);
    layout->addRow("Street Line 1:", streetLine1Edit);

    auto* streetLine2Edit = new QLineEdit(QString::fromStdString(contact.street_line_2), &dialog);
    streetLine2Edit->setReadOnly(!editable);
    layout->addRow("Street Line 2:", streetLine2Edit);

    auto* cityEdit = new QLineEdit(QString::fromStdString(contact.city), &dialog);
    cityEdit->setReadOnly(!editable);
    layout->addRow("City:", cityEdit);

    auto* stateEdit = new QLineEdit(QString::fromStdString(contact.state), &dialog);
    stateEdit->setReadOnly(!editable);
    layout->addRow("State:", stateEdit);

    auto* countryEdit = new QLineEdit(QString::fromStdString(contact.country_code), &dialog);
    countryEdit->setReadOnly(!editable);
    layout->addRow("Country Code:", countryEdit);

    auto* postalEdit = new QLineEdit(QString::fromStdString(contact.postal_code), &dialog);
    postalEdit->setReadOnly(!editable);
    layout->addRow("Postal Code:", postalEdit);

    auto* phoneEdit = new QLineEdit(QString::fromStdString(contact.phone), &dialog);
    phoneEdit->setReadOnly(!editable);
    layout->addRow("Phone:", phoneEdit);

    auto* emailEdit = new QLineEdit(QString::fromStdString(contact.email), &dialog);
    emailEdit->setReadOnly(!editable);
    layout->addRow("Email:", emailEdit);

    auto* webEdit = new QLineEdit(QString::fromStdString(contact.web_page), &dialog);
    webEdit->setReadOnly(!editable);
    layout->addRow("Web Page:", webEdit);

    auto buttons = editable
        ? QDialogButtonBox::Save | QDialogButtonBox::Cancel
        : QDialogButtonBox::Close;
    auto* buttonBox = new QDialogButtonBox(buttons, &dialog);
    layout->addRow(buttonBox);

    if (editable) {
        connect(buttonBox, &QDialogButtonBox::accepted, &dialog, &QDialog::accept);
        connect(buttonBox, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);
    } else {
        connect(buttonBox, &QDialogButtonBox::rejected, &dialog, &QDialog::reject);
    }

    if (dialog.exec() != QDialog::Accepted || !editable) return;

    // Save updated contact
    contact_entry updated = contact;
    updated.contact_type = typeEdit->text().trimmed().toStdString();
    updated.street_line_1 = streetLine1Edit->text().trimmed().toStdString();
    updated.street_line_2 = streetLine2Edit->text().trimmed().toStdString();
    updated.city = cityEdit->text().trimmed().toStdString();
    updated.state = stateEdit->text().trimmed().toStdString();
    updated.country_code = countryEdit->text().trimmed().toStdString();
    updated.postal_code = postalEdit->text().trimmed().toStdString();
    updated.phone = phoneEdit->text().trimmed().toStdString();
    updated.email = emailEdit->text().trimmed().toStdString();
    updated.web_page = webEdit->text().trimmed().toStdString();
    updated.modified_by = username_;
    updated.performed_by = username_;

    QPointer<EntityDetailDialog> self = this;
    auto* cm = clientManager_;
    auto ops = ops_;

    auto task = [cm, ops, updated]() -> operation_result {
        return ops->save_contact(cm, updated);
    };

    auto* watcher = new QFutureWatcher<operation_result>(self);
    connect(watcher, &QFutureWatcher<operation_result>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self) return;

        if (result.success) {
            self->loadContacts();
        } else {
            MessageBoxHelper::critical(self, "Save Failed",
                QString::fromStdString(result.message));
        }
    });

    QFuture<operation_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

// ============================================================================
// Save / Delete
// ============================================================================

void EntityDetailDialog::onSaveClicked() {
    const auto typeName = ops_->entity_type_name();
    const auto qTypeName = QString::fromStdString(typeName);

    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            QString("Cannot save %1 while disconnected from server.").arg(qTypeName.toLower()));
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateEntityFromUi();

    // For updates (not creates), require change reason
    if (!createMode_) {
        namespace reason = dq::domain::change_reason_constants;

        if (!changeReasonCache_ || !changeReasonCache_->isLoaded()) {
            BOOST_LOG_SEV(lg(), warn) << "Change reasons not loaded, cannot save.";
            emit errorMessage("Change reasons not loaded. Please try again.");
            return;
        }

        auto reasons = changeReasonCache_->getReasonsForAmend(
            std::string{reason::categories::common});
        if (reasons.empty()) {
            BOOST_LOG_SEV(lg(), warn) << "No change reasons available for common category.";
            emit errorMessage("No change reasons available. Please contact administrator.");
            return;
        }

        ChangeReasonDialog dialog(reasons, ChangeReasonDialog::OperationType::Amend,
            hasChanges_, this);
        if (dialog.exec() != QDialog::Accepted) {
            BOOST_LOG_SEV(lg(), debug) << "Save cancelled - change reason dialog rejected.";
            return;
        }

        entity_.change_reason_code = dialog.selectedReasonCode();
        entity_.change_commentary = dialog.commentary();

        BOOST_LOG_SEV(lg(), debug) << "Change reason selected: "
                                   << entity_.change_reason_code
                                   << ", commentary: " << entity_.change_commentary;
    }

    BOOST_LOG_SEV(lg(), info) << "Saving " << typeName << ": " << entity_.short_code;

    QPointer<EntityDetailDialog> self = this;
    auto ops = ops_;

    auto task = [self, ops, entity = entity_]() -> operation_result {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }
        return ops->save_entity(self->clientManager_, entity);
    };

    auto* watcher = new QFutureWatcher<operation_result>(self);
    connect(watcher, &QFutureWatcher<operation_result>::finished,
            self, [self, watcher, qTypeName]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << qTypeName.toStdString() << " saved successfully";
            QString code = QString::fromStdString(self->entity_.short_code);
            emit self->entitySaved(code);
            self->notifySaveSuccess(
                tr("%1 '%2' saved").arg(qTypeName).arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<operation_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void EntityDetailDialog::onDeleteClicked() {
    const auto typeName = ops_->entity_type_name();
    const auto qTypeName = QString::fromStdString(typeName);

    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            QString("Cannot delete %1 while disconnected from server.").arg(qTypeName.toLower()));
        return;
    }

    QString code = QString::fromStdString(entity_.short_code);
    auto reply = MessageBoxHelper::question(this,
        QString("Delete %1").arg(qTypeName),
        QString("Are you sure you want to delete %1 '%2'?").arg(qTypeName.toLower()).arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting " << typeName << ": " << entity_.short_code;

    QPointer<EntityDetailDialog> self = this;
    auto ops = ops_;

    auto task = [self, ops, id = entity_.id]() -> operation_result {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }
        return ops->delete_entity(self->clientManager_, id);
    };

    auto* watcher = new QFutureWatcher<operation_result>(self);
    connect(watcher, &QFutureWatcher<operation_result>::finished,
            self, [self, code, watcher, qTypeName]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << qTypeName.toStdString() << " deleted successfully";
            emit self->statusMessage(
                QString("%1 '%2' deleted").arg(qTypeName).arg(code));
            emit self->entityDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<operation_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
