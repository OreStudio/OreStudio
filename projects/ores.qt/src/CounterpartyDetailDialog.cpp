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
#include "ores.qt/CounterpartyDetailDialog.hpp"

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
#include "ui_CounterpartyDetailDialog.h"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/RelativeTimeHelper.hpp"
#include "ores.refdata/messaging/counterparty_protocol.hpp"
#include "ores.refdata/messaging/counterparty_identifier_protocol.hpp"
#include "ores.refdata/messaging/counterparty_contact_information_protocol.hpp"
#include "ores.refdata/messaging/party_id_scheme_protocol.hpp"
#include "ores.refdata/messaging/country_protocol.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/ChangeReasonCache.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.dq/domain/change_reason_constants.hpp"
#include "ores.comms/messaging/frame.hpp"

namespace ores::qt {

using namespace ores::logging;

CounterpartyDetailDialog::CounterpartyDetailDialog(QWidget* parent)
    : DetailDialogBase(parent),
      ui_(new Ui::CounterpartyDetailDialog),
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

CounterpartyDetailDialog::~CounterpartyDetailDialog() {
    delete ui_;
}

void CounterpartyDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));
}

void CounterpartyDetailDialog::setupIdentifierTable() {
    identifierToolbar_ = new QToolBar(this);
    identifierToolbar_->setIconSize(QSize(16, 16));

    auto* addAction = identifierToolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        "Add Identifier");
    auto* deleteAction = identifierToolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor),
        "Delete Identifier");

    connect(addAction, &QAction::triggered, this,
            &CounterpartyDetailDialog::onAddIdentifier);
    connect(deleteAction, &QAction::triggered, this,
            &CounterpartyDetailDialog::onDeleteIdentifier);

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

void CounterpartyDetailDialog::setupContactTable() {
    contactToolbar_ = new QToolBar(this);
    contactToolbar_->setIconSize(QSize(16, 16));

    auto* addAction = contactToolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Add, IconUtils::DefaultIconColor),
        "Add Contact");
    auto* deleteAction = contactToolbar_->addAction(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor),
        "Delete Contact");

    connect(addAction, &QAction::triggered, this,
            &CounterpartyDetailDialog::onAddContact);
    connect(deleteAction, &QAction::triggered, this,
            &CounterpartyDetailDialog::onDeleteContact);

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
            &CounterpartyDetailDialog::onContactDoubleClicked);
}

void CounterpartyDetailDialog::setupHierarchyTree() {
    hierarchyTree_ = new QTreeWidget(this);
    hierarchyTree_->setHeaderLabels({"Name", "Short Code", "Status"});
    hierarchyTree_->setColumnCount(3);
    hierarchyTree_->header()->setStretchLastSection(true);

    ui_->hierarchyLayout->addWidget(hierarchyTree_);
}

void CounterpartyDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this,
            &CounterpartyDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this,
            &CounterpartyDetailDialog::onDeleteClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this,
            &CounterpartyDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this,
            &CounterpartyDetailDialog::onFieldChanged);
    connect(ui_->transliteratedNameEdit, &QLineEdit::textChanged, this,
            &CounterpartyDetailDialog::onFieldChanged);
    connect(ui_->partyTypeCombo, &QComboBox::currentTextChanged, this,
            &CounterpartyDetailDialog::onFieldChanged);
    connect(ui_->parentCounterpartyCombo, &QComboBox::currentTextChanged, this,
            &CounterpartyDetailDialog::onFieldChanged);
    connect(ui_->statusCombo, &QComboBox::currentTextChanged, this,
            &CounterpartyDetailDialog::onFieldChanged);
    connect(ui_->businessCenterCombo, &QComboBox::currentTextChanged, this,
            &CounterpartyDetailDialog::onFieldChanged);
}

void CounterpartyDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateLookups();
}

void CounterpartyDetailDialog::setImageCache(ImageCache* imageCache) {
    imageCache_ = imageCache;
}

void CounterpartyDetailDialog::setChangeReasonCache(ChangeReasonCache* changeReasonCache) {
    changeReasonCache_ = changeReasonCache;
}

void CounterpartyDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CounterpartyDetailDialog::populateLookups() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        return;
    }

    QPointer<CounterpartyDetailDialog> self = this;
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

        self->updateUiFromCounterparty();

        // Also load all counterparties for parent combo + hierarchy
        self->loadAllCounterparties();
        self->loadIdSchemes();
        self->loadCountryImageMap();
    });

    QFuture<lookup_result> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CounterpartyDetailDialog::loadAllCounterparties() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        return;
    }

    QPointer<CounterpartyDetailDialog> self = this;
    auto* cm = clientManager_;

    struct CounterpartiesResult {
        std::vector<refdata::domain::counterparty> counterparties;
        bool success;
    };

    auto task = [cm]() -> CounterpartiesResult {
        refdata::messaging::get_counterparties_request request;
        request.offset = 0;
        request.limit = 1000;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_counterparties_request,
            0, std::move(payload)
        );

        auto response_result = cm->sendRequest(std::move(request_frame));
        if (!response_result) {
            return {{}, false};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {{}, false};
        }

        auto response = refdata::messaging::get_counterparties_response::
            deserialize(*payload_result);
        if (!response) {
            return {{}, false};
        }

        return {std::move(response->counterparties), true};
    };

    auto* watcher = new QFutureWatcher<CounterpartiesResult>(self);
    connect(watcher, &QFutureWatcher<CounterpartiesResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self || !result.success) return;

        self->allCounterparties_ = std::move(result.counterparties);
        self->populateParentCombo();
        self->buildHierarchyTree();
    });

    QFuture<CounterpartiesResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CounterpartyDetailDialog::loadIdSchemes() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<CounterpartyDetailDialog> self = this;
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

void CounterpartyDetailDialog::loadCountryImageMap() {
    if (!clientManager_ || !clientManager_->isConnected()) return;

    QPointer<CounterpartyDetailDialog> self = this;
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

void CounterpartyDetailDialog::populateParentCombo() {
    ui_->parentCounterpartyCombo->clear();
    ui_->parentCounterpartyCombo->addItem("(None)", QVariant());

    for (const auto& cpty : allCounterparties_) {
        // Don't allow setting self as parent
        if (cpty.id == counterparty_.id) continue;

        QString label = QString::fromStdString(cpty.short_code) + " - " +
                        QString::fromStdString(cpty.full_name);
        ui_->parentCounterpartyCombo->addItem(
            label, QString::fromStdString(boost::uuids::to_string(cpty.id)));
    }

    // Set current value
    if (counterparty_.parent_counterparty_id) {
        const auto parent_id_str = boost::uuids::to_string(*counterparty_.parent_counterparty_id);
        for (int i = 0; i < ui_->parentCounterpartyCombo->count(); ++i) {
            if (ui_->parentCounterpartyCombo->itemData(i).toString().toStdString() == parent_id_str) {
                ui_->parentCounterpartyCombo->setCurrentIndex(i);
                break;
            }
        }
    } else {
        ui_->parentCounterpartyCombo->setCurrentIndex(0);
    }
}

void CounterpartyDetailDialog::buildHierarchyTree() {
    hierarchyTree_->clear();

    if (allCounterparties_.empty()) return;

    // Build a map of id -> counterparty
    std::unordered_map<std::string, const refdata::domain::counterparty*> byId;
    for (const auto& cpty : allCounterparties_) {
        byId[boost::uuids::to_string(cpty.id)] = &cpty;
    }

    // Build a map of parent_id -> children
    std::unordered_map<std::string, std::vector<const refdata::domain::counterparty*>> children;
    std::vector<const refdata::domain::counterparty*> roots;

    for (const auto& cpty : allCounterparties_) {
        if (cpty.parent_counterparty_id) {
            children[boost::uuids::to_string(*cpty.parent_counterparty_id)].push_back(&cpty);
        } else {
            roots.push_back(&cpty);
        }
    }

    const auto current_id_str = boost::uuids::to_string(counterparty_.id);

    // Recursive tree builder
    std::function<QTreeWidgetItem*(const refdata::domain::counterparty*)> buildItem;
    buildItem = [&](const refdata::domain::counterparty* cpty) -> QTreeWidgetItem* {
        auto* item = new QTreeWidgetItem();
        const auto id_str = boost::uuids::to_string(cpty->id);
        const bool isCurrent = (id_str == current_id_str);

        QString name = QString::fromStdString(cpty->full_name);
        if (isCurrent) {
            name += " [THIS]";
        }
        item->setText(0, name);
        item->setText(1, QString::fromStdString(cpty->short_code));
        item->setText(2, QString::fromStdString(cpty->status));

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

void CounterpartyDetailDialog::setCounterparty(
    const refdata::domain::counterparty& counterparty) {
    counterparty_ = counterparty;
    updateUiFromCounterparty();
    loadIdentifiers();
    loadContacts();
}

void CounterpartyDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);

    if (createMode) {
        // Hide metadata tab - no data exists yet
        ui_->tabWidget->removeTab(ui_->tabWidget->indexOf(ui_->metadataTab));

        // Disable sub-entity toolbars until counterparty is saved
        if (identifierToolbar_) identifierToolbar_->setEnabled(false);
        if (contactToolbar_) contactToolbar_->setEnabled(false);
    }

    hasChanges_ = false;
    updateSaveButtonState();
}

void CounterpartyDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->transliteratedNameEdit->setReadOnly(readOnly);
    ui_->partyTypeCombo->setEnabled(!readOnly);
    ui_->parentCounterpartyCombo->setEnabled(!readOnly);
    ui_->statusCombo->setEnabled(!readOnly);
    ui_->businessCenterCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);

    if (identifierToolbar_) identifierToolbar_->setVisible(!readOnly);
    if (contactToolbar_) contactToolbar_->setVisible(!readOnly);
}

void CounterpartyDetailDialog::updateUiFromCounterparty() {
    ui_->codeEdit->setText(QString::fromStdString(counterparty_.short_code));
    ui_->nameEdit->setText(QString::fromStdString(counterparty_.full_name));
    ui_->transliteratedNameEdit->setText(
        QString::fromStdString(counterparty_.transliterated_name.value_or("")));
    ui_->partyTypeCombo->setCurrentText(QString::fromStdString(counterparty_.party_type));
    ui_->statusCombo->setCurrentText(QString::fromStdString(counterparty_.status));
    ui_->businessCenterCombo->setCurrentText(QString::fromStdString(counterparty_.business_center_code));

    ui_->versionEdit->setText(QString::number(counterparty_.version));
    ui_->modifiedByEdit->setText(QString::fromStdString(counterparty_.modified_by));
    ui_->recordedAtEdit->setText(relative_time_helper::format(counterparty_.recorded_at));
    ui_->commentaryEdit->setText(QString::fromStdString(counterparty_.change_commentary));
}

void CounterpartyDetailDialog::updateCounterpartyFromUi() {
    if (createMode_) {
        counterparty_.short_code = ui_->codeEdit->text().trimmed().toStdString();
    }
    counterparty_.full_name = ui_->nameEdit->text().trimmed().toStdString();
    const auto tlit = ui_->transliteratedNameEdit->text().trimmed().toStdString();
    counterparty_.transliterated_name = tlit.empty() ? std::nullopt : std::optional<std::string>(tlit);
    counterparty_.party_type = ui_->partyTypeCombo->currentText().trimmed().toStdString();
    counterparty_.status = ui_->statusCombo->currentText().trimmed().toStdString();
    counterparty_.business_center_code = ui_->businessCenterCombo->currentText().trimmed().toStdString();
    counterparty_.modified_by = username_;
    counterparty_.performed_by = username_;

    // Parent counterparty
    int parentIndex = ui_->parentCounterpartyCombo->currentIndex();
    QVariant parentData = ui_->parentCounterpartyCombo->itemData(parentIndex);
    if (parentData.isValid() && !parentData.toString().isEmpty()) {
        auto parent_id_str = parentData.toString().toStdString();
        boost::uuids::string_generator gen;
        counterparty_.parent_counterparty_id = gen(parent_id_str);
    } else {
        counterparty_.parent_counterparty_id = std::nullopt;
    }
}

void CounterpartyDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CounterpartyDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CounterpartyDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CounterpartyDetailDialog::validateInput() {
    const QString short_code_val = ui_->codeEdit->text().trimmed();
    const QString full_name_val = ui_->nameEdit->text().trimmed();

    return !short_code_val.isEmpty() && !full_name_val.isEmpty();
}

// ============================================================================
// Identifier sub-table
// ============================================================================

void CounterpartyDetailDialog::loadIdentifiers() {
    if (!clientManager_ || !clientManager_->isConnected()) return;
    if (counterparty_.id.is_nil()) return;

    QPointer<CounterpartyDetailDialog> self = this;
    auto* cm = clientManager_;
    auto cpty_id = counterparty_.id;

    struct IdentifiersResult {
        std::vector<refdata::domain::counterparty_identifier> identifiers;
        bool success;
    };

    auto task = [cm, cpty_id]() -> IdentifiersResult {
        refdata::messaging::get_counterparty_identifiers_request request;
        request.counterparty_id = cpty_id;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_counterparty_identifiers_request,
            0, std::move(payload)
        );

        auto response_result = cm->sendRequest(std::move(request_frame));
        if (!response_result) return {{}, false};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {{}, false};

        auto response = refdata::messaging::get_counterparty_identifiers_response::
            deserialize(*payload_result);
        if (!response) return {{}, false};

        return {std::move(response->counterparty_identifiers), true};
    };

    auto* watcher = new QFutureWatcher<IdentifiersResult>(self);
    connect(watcher, &QFutureWatcher<IdentifiersResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self || !result.success) return;

        self->identifiers_ = std::move(result.identifiers);
        self->populateIdentifierTable();
    });

    QFuture<IdentifiersResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CounterpartyDetailDialog::populateIdentifierTable() {
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

void CounterpartyDetailDialog::onAddIdentifier() {
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

    QPointer<CounterpartyDetailDialog> self = this;
    auto* cm = clientManager_;

    refdata::domain::counterparty_identifier newIdent;
    boost::uuids::random_generator uuid_gen;
    newIdent.id = uuid_gen();
    newIdent.counterparty_id = counterparty_.id;
    newIdent.id_scheme = schemeCombo->currentData().toString().toStdString();
    newIdent.id_value = valueEdit->text().trimmed().toStdString();
    newIdent.description = descEdit->text().trimmed().toStdString();
    newIdent.modified_by = username_;
    newIdent.performed_by = username_;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [cm, newIdent]() -> SaveResult {
        refdata::messaging::save_counterparty_identifier_request request;
        request.counterparty_identifier = newIdent;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_counterparty_identifier_request,
            0, std::move(payload)
        );

        auto response_result = cm->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = refdata::messaging::save_counterparty_identifier_response::
            deserialize(*payload_result);
        if (!response) return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
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

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CounterpartyDetailDialog::onDeleteIdentifier() {
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

    QPointer<CounterpartyDetailDialog> self = this;
    auto* cm = clientManager_;
    auto identId = ident.id;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [cm, identId]() -> DeleteResult {
        refdata::messaging::delete_counterparty_identifier_request request;
        request.ids = {identId};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_counterparty_identifier_request,
            0, std::move(payload)
        );

        auto response_result = cm->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = refdata::messaging::delete_counterparty_identifier_response::
            deserialize(*payload_result);
        if (!response || response->results.empty()) return {false, "Invalid server response"};

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
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

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

// ============================================================================
// Contact sub-table
// ============================================================================

void CounterpartyDetailDialog::loadContacts() {
    if (!clientManager_ || !clientManager_->isConnected()) return;
    if (counterparty_.id.is_nil()) return;

    QPointer<CounterpartyDetailDialog> self = this;
    auto* cm = clientManager_;
    auto cpty_id = counterparty_.id;

    struct ContactsResult {
        std::vector<refdata::domain::counterparty_contact_information> contacts;
        bool success;
    };

    auto task = [cm, cpty_id]() -> ContactsResult {
        refdata::messaging::get_counterparty_contact_informations_request request;
        request.counterparty_id = cpty_id;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::get_counterparty_contact_informations_request,
            0, std::move(payload)
        );

        auto response_result = cm->sendRequest(std::move(request_frame));
        if (!response_result) return {{}, false};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {{}, false};

        auto response = refdata::messaging::get_counterparty_contact_informations_response::
            deserialize(*payload_result);
        if (!response) return {{}, false};

        return {std::move(response->counterparty_contact_informations), true};
    };

    auto* watcher = new QFutureWatcher<ContactsResult>(self);
    connect(watcher, &QFutureWatcher<ContactsResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (!self || !result.success) return;

        self->contacts_ = std::move(result.contacts);
        self->populateContactTable();
    });

    QFuture<ContactsResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CounterpartyDetailDialog::populateContactTable() {
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

void CounterpartyDetailDialog::onAddContact() {
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

    QPointer<CounterpartyDetailDialog> self = this;
    auto* cm = clientManager_;

    refdata::domain::counterparty_contact_information newContact;
    boost::uuids::random_generator uuid_gen;
    newContact.id = uuid_gen();
    newContact.counterparty_id = counterparty_.id;
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

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [cm, newContact]() -> SaveResult {
        refdata::messaging::save_counterparty_contact_information_request request;
        request.counterparty_contact_information = newContact;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_counterparty_contact_information_request,
            0, std::move(payload)
        );

        auto response_result = cm->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = refdata::messaging::save_counterparty_contact_information_response::
            deserialize(*payload_result);
        if (!response) return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
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

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CounterpartyDetailDialog::onDeleteContact() {
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

    QPointer<CounterpartyDetailDialog> self = this;
    auto* cm = clientManager_;
    auto contactId = contact.id;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [cm, contactId]() -> DeleteResult {
        refdata::messaging::delete_counterparty_contact_information_request request;
        request.ids = {contactId};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_counterparty_contact_information_request,
            0, std::move(payload)
        );

        auto response_result = cm->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = refdata::messaging::delete_counterparty_contact_information_response::
            deserialize(*payload_result);
        if (!response || response->results.empty()) return {false, "Invalid server response"};

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
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

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CounterpartyDetailDialog::onContactDoubleClicked(int row, int /* column */) {
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
    refdata::domain::counterparty_contact_information updated = contact;
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

    QPointer<CounterpartyDetailDialog> self = this;
    auto* cm = clientManager_;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [cm, updated]() -> SaveResult {
        refdata::messaging::save_counterparty_contact_information_request request;
        request.counterparty_contact_information = updated;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_counterparty_contact_information_request,
            0, std::move(payload)
        );

        auto response_result = cm->sendRequest(std::move(request_frame));
        if (!response_result) return {false, "Failed to communicate with server"};

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) return {false, "Failed to decompress response"};

        auto response = refdata::messaging::save_counterparty_contact_information_response::
            deserialize(*payload_result);
        if (!response) return {false, "Invalid server response"};

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
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

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

// ============================================================================
// Save / Delete
// ============================================================================

void CounterpartyDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot save counterparty while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input",
            "Please fill in all required fields.");
        return;
    }

    updateCounterpartyFromUi();

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

        counterparty_.change_reason_code = dialog.selectedReasonCode();
        counterparty_.change_commentary = dialog.commentary();

        BOOST_LOG_SEV(lg(), debug) << "Change reason selected: "
                                   << counterparty_.change_reason_code
                                   << ", commentary: " << counterparty_.change_commentary;
    }

    BOOST_LOG_SEV(lg(), info) << "Saving counterparty: " << counterparty_.short_code;

    QPointer<CounterpartyDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, counterparty = counterparty_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_counterparty_request request;
        request.counterparty = counterparty;
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::save_counterparty_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = refdata::messaging::save_counterparty_response::
            deserialize(*payload_result);

        if (!response) {
            return {false, "Invalid server response"};
        }

        return {response->success, response->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished,
            self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Counterparty saved successfully";
            QString code = QString::fromStdString(self->counterparty_.short_code);
            emit self->counterpartySaved(code);
            self->notifySaveSuccess(tr("Counterparty '%1' saved").arg(code));
        } else {
            BOOST_LOG_SEV(lg(), error) << "Save failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Save Failed", errorMsg);
        }
    });

    QFuture<SaveResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

void CounterpartyDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(this, "Disconnected",
            "Cannot delete counterparty while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(counterparty_.short_code);
    auto reply = MessageBoxHelper::question(this, "Delete Counterparty",
        QString("Are you sure you want to delete counterparty '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    BOOST_LOG_SEV(lg(), info) << "Deleting counterparty: " << counterparty_.short_code;

    QPointer<CounterpartyDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id = counterparty_.id]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_counterparty_request request;
        request.ids = {id};
        auto payload = request.serialize();

        comms::messaging::frame request_frame(
            comms::messaging::message_type::delete_counterparty_request,
            0, std::move(payload)
        );

        auto response_result = self->clientManager_->sendRequest(
            std::move(request_frame));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        auto payload_result = response_result->decompressed_payload();
        if (!payload_result) {
            return {false, "Failed to decompress response"};
        }

        auto response = refdata::messaging::delete_counterparty_response::
            deserialize(*payload_result);

        if (!response || response->results.empty()) {
            return {false, "Invalid server response"};
        }

        return {response->results[0].success, response->results[0].message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished,
            self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Counterparty deleted successfully";
            emit self->statusMessage(QString("Counterparty '%1' deleted").arg(code));
            emit self->counterpartyDeleted(code);
            self->requestClose();
        } else {
            BOOST_LOG_SEV(lg(), error) << "Delete failed: " << result.message;
            QString errorMsg = QString::fromStdString(result.message);
            emit self->errorMessage(errorMsg);
            MessageBoxHelper::critical(self, "Delete Failed", errorMsg);
        }
    });

    QFuture<DeleteResult> future = QtConcurrent::run(task);
    watcher->setFuture(future);
}

}
