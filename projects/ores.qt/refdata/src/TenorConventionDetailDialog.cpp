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
#include "ores.qt/TenorConventionDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/tenor_convention_protocol.hpp"
#include "ui_TenorConventionDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

TenorConventionDetailDialog::TenorConventionDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::TenorConventionDetailDialog)
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

TenorConventionDetailDialog::~TenorConventionDetailDialog() {
    delete ui_;
}

QTabWidget* TenorConventionDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* TenorConventionDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* TenorConventionDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString TenorConventionDetailDialog::code() const {
    return QString::fromStdString(convention_.code);
}

void TenorConventionDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void TenorConventionDetailDialog::setupCombos() {}

void TenorConventionDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &TenorConventionDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &TenorConventionDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &TenorConventionDetailDialog::onCloseClicked);

    connect(
        ui_->codeEdit, &QLineEdit::textChanged, this, &TenorConventionDetailDialog::onCodeChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &TenorConventionDetailDialog::onFieldChanged);
    connect(ui_->measuredFromCombo,
            &QComboBox::currentIndexChanged,
            this,
            &TenorConventionDetailDialog::onFieldChanged);
    connect(ui_->resolutionAlgorithmCombo,
            &QComboBox::currentIndexChanged,
            this,
            &TenorConventionDetailDialog::onFieldChanged);
}

void TenorConventionDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateMeasuredFromCombo();
    populateResolutionAlgorithmCombo();
}

void TenorConventionDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void TenorConventionDetailDialog::setConvention(
    const refdata::domain::tenor_convention& convention) {
    convention_ = convention;
    updateUiFromConvention();
}

void TenorConventionDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void TenorConventionDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorConventionDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->measuredFromCombo->setEnabled(!readOnly);
    ui_->resolutionAlgorithmCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void TenorConventionDetailDialog::populateMeasuredFromCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating measured_from combo";
    populateDynamicCombo<refdata::domain::tenor_anchor>(
        ui_->measuredFromCombo,
        this,
        clientManager_,
        &fetch_tenor_anchors,
        "measuredFromWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(convention_.measured_from); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load tenor anchors: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; },
        QString{});
}
void TenorConventionDetailDialog::populateResolutionAlgorithmCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating resolution_algorithm combo";
    populateDynamicCombo<refdata::domain::tenor_resolution_algorithm>(
        ui_->resolutionAlgorithmCombo,
        this,
        clientManager_,
        &fetch_tenor_resolution_algorithms,
        "resolutionAlgorithmWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(convention_.resolution_algorithm); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load tenor resolution algorithms: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; },
        QString{});
}
void TenorConventionDetailDialog::updateUiFromConvention() {
    ui_->codeEdit->setText(QString::fromStdString(convention_.code));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(convention_.description));
    {
        const auto val = QString::fromStdString(convention_.measured_from);
        const int idx = ui_->measuredFromCombo->findData(val);
        if (idx >= 0)
            ui_->measuredFromCombo->setCurrentIndex(idx);
    }
    {
        const auto val = QString::fromStdString(convention_.resolution_algorithm);
        const int idx = ui_->resolutionAlgorithmCombo->findData(val);
        if (idx >= 0)
            ui_->resolutionAlgorithmCombo->setCurrentIndex(idx);
    }

    populateProvenance(convention_.version,
                       convention_.modified_by,
                       convention_.performed_by,
                       convention_.recorded_at,
                       convention_.change_reason_code,
                       convention_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void TenorConventionDetailDialog::updateConventionFromUi() {
    if (createMode_) {
        convention_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    convention_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    convention_.measured_from = ui_->measuredFromCombo->currentText().toStdString();
    convention_.resolution_algorithm = ui_->resolutionAlgorithmCombo->currentText().toStdString();
    convention_.modified_by = username_;
}

void TenorConventionDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorConventionDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void TenorConventionDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool TenorConventionDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const bool measured_from_selected = ui_->measuredFromCombo->currentIndex() >= 0;
    const bool resolution_algorithm_selected = ui_->resolutionAlgorithmCombo->currentIndex() >= 0;

    return true && !code_val.isEmpty() && measured_from_selected && resolution_algorithm_selected;
}

void TenorConventionDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save tenor convention while disconnected from server.");
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
    convention_.change_reason_code = crSel->reason_code;
    convention_.change_commentary = crSel->commentary;

    updateConventionFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving tenor convention: " << convention_.code;

    QPointer<TenorConventionDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, convention = convention_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_tenor_convention_request request;
        request.data = convention;
        auto response_result =
            self->clientManager_->process_authenticated_request(std::move(request));

        if (!response_result) {
            return {false, "Failed to communicate with server"};
        }

        return {response_result->success, response_result->message};
    };

    auto* watcher = new QFutureWatcher<SaveResult>(self);
    connect(watcher, &QFutureWatcher<SaveResult>::finished, self, [self, watcher]() {
        auto result = watcher->result();
        watcher->deleteLater();

        if (result.success) {
            BOOST_LOG_SEV(lg(), info) << "Tenor Convention saved successfully";
            QString code = QString::fromStdString(self->convention_.code);
            self->hasChanges_ = false;
            self->updateSaveButtonState();
            emit self->conventionSaved(code);
            self->notifySaveSuccess(tr("Tenor Convention '%1' saved").arg(code));
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

void TenorConventionDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete tenor convention while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(convention_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Tenor Convention",
        QString("Are you sure you want to delete tenor convention '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting tenor convention: " << convention_.code;

    QPointer<TenorConventionDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = convention_.code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_tenor_convention_request request;
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
            BOOST_LOG_SEV(lg(), info) << "Tenor Convention deleted successfully";
            emit self->statusMessage(QString("Tenor Convention '%1' deleted").arg(code));
            emit self->conventionDeleted(code);
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
