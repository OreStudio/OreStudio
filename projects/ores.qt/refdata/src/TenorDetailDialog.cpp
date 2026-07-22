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
#include "ores.qt/TenorDetailDialog.hpp"
#include "ores.qt/BadgeComboHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/tenor_protocol.hpp"
#include "ui_TenorDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

TenorDetailDialog::TenorDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::TenorDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    WidgetUtils::setupComboBoxes(this);
    setupUi();
    setupCombos();
    setupConnections();
    // Hierarchy tree seam: a future :implements 9B165431-2921-4CAC-A2E8-2C186741E523
    // block is expected to construct a HierarchyModelBuilder-derived model
    // for this entity, wrap it in a HierarchyTreeWidget, and insert that
    // widget into this dialog's layout (e.g. a dedicated tab). Left empty
    // when no entity implements this kind.
    // Composite child-entity tables seam: an :implements
    // 7E4A2C8D-9F1B-4E6A-8D3C-5B2A7E9F1C4D block constructs one QTableWidget
    // + QToolBar per embedded child entity (e.g. identifiers, contact
    // information), wraps each in a tab, and inserts it into this dialog's
    // tab widget. Left empty when no entity implements this kind.
}

TenorDetailDialog::~TenorDetailDialog() {
    delete ui_;
}

QTabWidget* TenorDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* TenorDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* TenorDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString TenorDetailDialog::code() const {
    return QString::fromStdString(tenor_.code);
}

void TenorDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void TenorDetailDialog::setupCombos() {}

void TenorDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &TenorDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &TenorDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &TenorDetailDialog::onCloseClicked);

    connect(ui_->codeEdit, &QLineEdit::textChanged, this, &TenorDetailDialog::onCodeChanged);
    connect(
        ui_->displayNameEdit, &QLineEdit::textChanged, this, &TenorDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &TenorDetailDialog::onFieldChanged);
    connect(ui_->sortOrderEdit, &QSpinBox::valueChanged, this, &TenorDetailDialog::onFieldChanged);
    connect(
        ui_->kindCombo, &QComboBox::currentIndexChanged, this, &TenorDetailDialog::onFieldChanged);
    connect(
        ui_->unitCombo, &QComboBox::currentIndexChanged, this, &TenorDetailDialog::onFieldChanged);
    connect(ui_->multiplierEdit, &QSpinBox::valueChanged, this, &TenorDetailDialog::onFieldChanged);
}

void TenorDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateKindCombo();
    setup_badge_combo(this, ui_->kindCombo, badgeCache(), "tenor_kind");
    populateUnitCombo();
    setup_badge_combo(this, ui_->unitCombo, badgeCache(), "tenor_unit");
}

void TenorDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void TenorDetailDialog::setTenor(const refdata::domain::tenor& tenor) {
    tenor_ = tenor;
    updateUiFromTenor();
}

void TenorDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void TenorDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->displayNameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->kindCombo->setEnabled(!readOnly);
    ui_->unitCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void TenorDetailDialog::populateKindCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating kind combo";
    populateDynamicCombo<refdata::domain::tenor_kind>(
        ui_->kindCombo,
        this,
        clientManager_,
        &fetch_tenor_kinds,
        "tenorKindWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(tenor_.kind); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load tenor kinds: %1").arg(error));
        },
        [this]() { setup_badge_combo(this, ui_->kindCombo, badgeCache(), "tenor_kind"); },
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; },
        QString{});
}
void TenorDetailDialog::populateUnitCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating unit combo";
    populateDynamicCombo<refdata::domain::tenor_unit>(
        ui_->unitCombo,
        this,
        clientManager_,
        &fetch_tenor_units,
        "tenorUnitWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(tenor_.unit); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load tenor units: %1").arg(error));
        },
        [this]() { setup_badge_combo(this, ui_->unitCombo, badgeCache(), "tenor_unit"); },
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; },
        QString{});
}
void TenorDetailDialog::updateUiFromTenor() {
    ui_->codeEdit->setText(QString::fromStdString(tenor_.code));
    ui_->displayNameEdit->setText(QString::fromStdString(tenor_.display_name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(tenor_.description));
    ui_->sortOrderEdit->setValue(tenor_.sort_order);
    {
        const auto val = QString::fromStdString(tenor_.kind);
        const int idx = ui_->kindCombo->findData(val);
        if (idx >= 0)
            ui_->kindCombo->setCurrentIndex(idx);
    }
    {
        const auto val = QString::fromStdString(tenor_.unit);
        const int idx = ui_->unitCombo->findData(val);
        if (idx >= 0)
            ui_->unitCombo->setCurrentIndex(idx);
    }
    ui_->multiplierEdit->setValue(tenor_.multiplier.value_or(ui_->multiplierEdit->minimum()));

    populateProvenance(tenor_.version,
                       tenor_.modified_by,
                       tenor_.performed_by,
                       tenor_.recorded_at,
                       tenor_.change_reason_code,
                       tenor_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void TenorDetailDialog::updateTenorFromUi() {
    if (createMode_) {
        tenor_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    tenor_.display_name = ui_->displayNameEdit->text().trimmed().toStdString();
    tenor_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    tenor_.sort_order = ui_->sortOrderEdit->value();
    tenor_.kind = ui_->kindCombo->currentText().toStdString();
    tenor_.unit = ui_->unitCombo->currentText().toStdString();
    if (ui_->multiplierEdit->value() == ui_->multiplierEdit->minimum())
        tenor_.multiplier = std::nullopt;
    else
        tenor_.multiplier = ui_->multiplierEdit->value();
    tenor_.modified_by = username_;
}

void TenorDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool TenorDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString display_name_val = ui_->displayNameEdit->text().trimmed();
    const bool kind_selected = ui_->kindCombo->currentIndex() >= 0;
    const bool unit_selected = ui_->unitCombo->currentIndex() >= 0;

    return true && !code_val.isEmpty() && !display_name_val.isEmpty() && kind_selected &&
           unit_selected;
}

void TenorDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save tenor while disconnected from server.");
        return;
    }

    if (!validateInput()) {
        MessageBoxHelper::warning(this, "Invalid Input", "Please fill in all required fields.");
        return;
    }


    const auto crOpType = createMode_ ? ChangeReasonDialog::OperationType::Create :
                                        ChangeReasonDialog::OperationType::Amend;
    const auto crSel = promptChangeReason(crOpType, hasChanges_, createMode_ ? "system" : "common");
    if (!crSel)
        return;
    tenor_.change_reason_code = crSel->reason_code;
    tenor_.change_commentary = crSel->commentary;

    updateTenorFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving tenor: " << tenor_.code;

    QPointer<TenorDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, tenor = tenor_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_tenor_request request;
        request.data = tenor;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher,
            &QFutureWatcher<SaveResult>::finished,
            self,
            [self, watcher, crReasonCode = crSel->reason_code, crCommentary = crSel->commentary]() {
                auto result = watcher->result();
                watcher->deleteLater();

                if (result.success) {
                    BOOST_LOG_SEV(lg(), info) << "Tenor saved successfully";
                    QString code = QString::fromStdString(self->tenor_.code);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->tenorSaved(code);
                    self->notifySaveSuccess(tr("Tenor '%1' saved").arg(code));
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

void TenorDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete tenor while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(tenor_.code);
    auto reply =
        MessageBoxHelper::question(this,
                                   "Delete Tenor",
                                   QString("Are you sure you want to delete tenor '%1'?").arg(code),
                                   QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting tenor: " << tenor_.code;

    QPointer<TenorDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = tenor_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_tenor_request request;
        request.codes = {code};
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<DeleteResult>(self);
    connect(watcher, &QFutureWatcher<DeleteResult>::finished, self, [self, code, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Tenor deleted successfully";
            emit self->statusMessage(QString("Tenor '%1' deleted").arg(code));
            emit self->tenorDeleted(code);
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
