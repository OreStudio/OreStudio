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
#include "ores.qt/DatasetBundleDetailDialog.hpp"
#include "ores.dq.api/messaging/dataset_bundle_protocol.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ui_DatasetBundleDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

DatasetBundleDetailDialog::DatasetBundleDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::DatasetBundleDetailDialog)
    , clientManager_(nullptr) {

    ui_->setupUi(this);
    setupUi();
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

DatasetBundleDetailDialog::~DatasetBundleDetailDialog() {
    delete ui_;
}

QTabWidget* DatasetBundleDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* DatasetBundleDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* DatasetBundleDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString DatasetBundleDetailDialog::code() const {
    return QString::fromStdString(bundle_.code);
}

void DatasetBundleDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void DatasetBundleDetailDialog::setupConnections() {
    connect(
        ui_->saveButton, &QPushButton::clicked, this, &DatasetBundleDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &DatasetBundleDetailDialog::onDeleteClicked);
    connect(
        ui_->closeButton, &QPushButton::clicked, this, &DatasetBundleDetailDialog::onCloseClicked);

    connect(
        ui_->codeEdit, &QLineEdit::textChanged, this, &DatasetBundleDetailDialog::onCodeChanged);
    connect(
        ui_->nameEdit, &QLineEdit::textChanged, this, &DatasetBundleDetailDialog::onFieldChanged);
    connect(ui_->descriptionEdit,
            &QPlainTextEdit::textChanged,
            this,
            &DatasetBundleDetailDialog::onFieldChanged);
}

void DatasetBundleDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void DatasetBundleDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void DatasetBundleDetailDialog::setBundle(const dq::domain::dataset_bundle& bundle) {
    bundle_ = bundle;
    updateUiFromBundle();
}

void DatasetBundleDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->codeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        bundle_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void DatasetBundleDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void DatasetBundleDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->codeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->descriptionEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void DatasetBundleDetailDialog::updateUiFromBundle() {
    ui_->codeEdit->setText(QString::fromStdString(bundle_.code));
    ui_->nameEdit->setText(QString::fromStdString(bundle_.name));
    ui_->descriptionEdit->setPlainText(QString::fromStdString(bundle_.description));

    populateProvenance(bundle_.version,
                       bundle_.modified_by,
                       bundle_.performed_by,
                       bundle_.recorded_at,
                       bundle_.change_reason_code,
                       bundle_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void DatasetBundleDetailDialog::updateBundleFromUi() {
    if (createMode_) {
        bundle_.code = ui_->codeEdit->text().trimmed().toStdString();
    }
    bundle_.name = ui_->nameEdit->text().trimmed().toStdString();
    bundle_.description = ui_->descriptionEdit->toPlainText().trimmed().toStdString();
    bundle_.modified_by = username_;
}

void DatasetBundleDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void DatasetBundleDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void DatasetBundleDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool DatasetBundleDetailDialog::validateInput() {
    const QString code_val = ui_->codeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();

    return true && !code_val.isEmpty() && !name_val.isEmpty();
}

void DatasetBundleDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save dataset bundle while disconnected from server.");
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
    bundle_.change_reason_code = crSel->reason_code;
    bundle_.change_commentary = crSel->commentary;

    updateBundleFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving dataset bundle: " << bundle_.code;

    QPointer<DatasetBundleDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, bundle = bundle_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::save_dataset_bundle_request request;
        request.data = bundle;
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
                    BOOST_LOG_SEV(lg(), info) << "Dataset Bundle saved successfully";
                    QString code = QString::fromStdString(self->bundle_.code);
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->bundleSaved(code);
                    self->notifySaveSuccess(tr("Dataset Bundle '%1' saved").arg(code));
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

void DatasetBundleDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete dataset bundle while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(bundle_.code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Dataset Bundle",
        QString("Are you sure you want to delete dataset bundle '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting dataset bundle: " << bundle_.code;

    QPointer<DatasetBundleDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(bundle_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        dq::messaging::delete_dataset_bundle_request request;
        request.ids = {id_str};
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
            BOOST_LOG_SEV(lg(), info) << "Dataset Bundle deleted successfully";
            emit self->statusMessage(QString("Dataset Bundle '%1' deleted").arg(code));
            emit self->bundleDeleted(code);
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
