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
#include "ores.qt/IrCurveBootstrapConfigDetailDialog.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.refdata.api/messaging/ir_curve_bootstrap_config_protocol.hpp"
#include "ui_IrCurveBootstrapConfigDetailDialog.h"
#include <QFutureWatcher>
#include <QMessageBox>
#include <QtConcurrent>
#include <boost/uuid/random_generator.hpp>
#include <boost/uuid/string_generator.hpp>
#include <boost/uuid/uuid_io.hpp>

namespace ores::qt {

using namespace ores::logging;

IrCurveBootstrapConfigDetailDialog::IrCurveBootstrapConfigDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::IrCurveBootstrapConfigDetailDialog)
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

IrCurveBootstrapConfigDetailDialog::~IrCurveBootstrapConfigDetailDialog() {
    delete ui_;
}

QTabWidget* IrCurveBootstrapConfigDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* IrCurveBootstrapConfigDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* IrCurveBootstrapConfigDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString IrCurveBootstrapConfigDetailDialog::code() const {
    return QString::fromStdString(boost::uuids::to_string(config_.id));
}

void IrCurveBootstrapConfigDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));
}

void IrCurveBootstrapConfigDetailDialog::setupConnections() {
    connect(ui_->saveButton,
            &QPushButton::clicked,
            this,
            &IrCurveBootstrapConfigDetailDialog::onSaveClicked);
    connect(ui_->deleteButton,
            &QPushButton::clicked,
            this,
            &IrCurveBootstrapConfigDetailDialog::onDeleteClicked);
    connect(ui_->closeButton,
            &QPushButton::clicked,
            this,
            &IrCurveBootstrapConfigDetailDialog::onCloseClicked);

    connect(ui_->idEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveBootstrapConfigDetailDialog::onCodeChanged);
    connect(ui_->sourceSeriesIdEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveBootstrapConfigDetailDialog::onFieldChanged);
    connect(ui_->outputSeriesIdEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveBootstrapConfigDetailDialog::onFieldChanged);
    connect(ui_->curveFamilyRoleEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveBootstrapConfigDetailDialog::onFieldChanged);
    connect(ui_->discountCurveConfigIdEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveBootstrapConfigDetailDialog::onFieldChanged);
    connect(ui_->interpolationMethodEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveBootstrapConfigDetailDialog::onFieldChanged);
    connect(ui_->dayCountConventionEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveBootstrapConfigDetailDialog::onFieldChanged);
    connect(ui_->splitTenorCodeEdit,
            &QLineEdit::textChanged,
            this,
            &IrCurveBootstrapConfigDetailDialog::onFieldChanged);
}

void IrCurveBootstrapConfigDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
}

void IrCurveBootstrapConfigDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void IrCurveBootstrapConfigDetailDialog::setConfig(
    const refdata::domain::ir_curve_bootstrap_config& config) {
    config_ = config;
    updateUiFromConfig();
}

void IrCurveBootstrapConfigDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->idEdit->setReadOnly(true);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    if (createMode) {
        config_.id = boost::uuids::random_generator()();
    }
    hasChanges_ = false;
    updateSaveButtonState();
}

void IrCurveBootstrapConfigDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void IrCurveBootstrapConfigDetailDialog::setReadOnly(bool readOnly) {
    readOnly_ = readOnly;
    ui_->idEdit->setReadOnly(true);
    ui_->sourceSeriesIdEdit->setReadOnly(readOnly);
    ui_->outputSeriesIdEdit->setReadOnly(readOnly);
    ui_->curveFamilyRoleEdit->setReadOnly(readOnly);
    ui_->discountCurveConfigIdEdit->setReadOnly(readOnly);
    ui_->interpolationMethodEdit->setReadOnly(readOnly);
    ui_->dayCountConventionEdit->setReadOnly(readOnly);
    ui_->splitTenorCodeEdit->setReadOnly(readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
}

void IrCurveBootstrapConfigDetailDialog::updateUiFromConfig() {
    ui_->idEdit->setText(QString::fromStdString(boost::uuids::to_string(config_.id)));
    ui_->sourceSeriesIdEdit->setText(
        QString::fromStdString(boost::uuids::to_string(config_.source_series_id)));
    ui_->outputSeriesIdEdit->setText(
        QString::fromStdString(boost::uuids::to_string(config_.output_series_id)));
    ui_->curveFamilyRoleEdit->setText(QString::fromStdString(config_.curve_family_role));
    ui_->discountCurveConfigIdEdit->setText(
        QString::fromStdString(boost::uuids::to_string(config_.discount_curve_config_id)));
    ui_->interpolationMethodEdit->setText(QString::fromStdString(config_.interpolation_method));
    ui_->dayCountConventionEdit->setText(QString::fromStdString(config_.day_count_convention));
    ui_->splitTenorCodeEdit->setText(QString::fromStdString(config_.split_tenor_code));

    populateProvenance(config_.version,
                       config_.modified_by,
                       config_.performed_by,
                       config_.recorded_at,
                       config_.change_reason_code,
                       config_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void IrCurveBootstrapConfigDetailDialog::updateConfigFromUi() {
    config_.curve_family_role = ui_->curveFamilyRoleEdit->text().trimmed().toStdString();
    config_.interpolation_method = ui_->interpolationMethodEdit->text().trimmed().toStdString();
    config_.day_count_convention = ui_->dayCountConventionEdit->text().trimmed().toStdString();
    config_.split_tenor_code = ui_->splitTenorCodeEdit->text().trimmed().toStdString();
    config_.modified_by = username_;
}

void IrCurveBootstrapConfigDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void IrCurveBootstrapConfigDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void IrCurveBootstrapConfigDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool IrCurveBootstrapConfigDetailDialog::validateInput() {
    const QString id_val = ui_->idEdit->text().trimmed();
    const QString source_series_id_val = ui_->sourceSeriesIdEdit->text().trimmed();
    const QString output_series_id_val = ui_->outputSeriesIdEdit->text().trimmed();
    const QString curve_family_role_val = ui_->curveFamilyRoleEdit->text().trimmed();
    const QString interpolation_method_val = ui_->interpolationMethodEdit->text().trimmed();
    const QString day_count_convention_val = ui_->dayCountConventionEdit->text().trimmed();
    const QString split_tenor_code_val = ui_->splitTenorCodeEdit->text().trimmed();

    return true && !id_val.isEmpty() && !source_series_id_val.isEmpty() &&
           !output_series_id_val.isEmpty() && !curve_family_role_val.isEmpty() &&
           !interpolation_method_val.isEmpty() && !day_count_convention_val.isEmpty() &&
           !split_tenor_code_val.isEmpty();
}

void IrCurveBootstrapConfigDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot save IR curve bootstrap config while disconnected from server.");
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
    config_.change_reason_code = crSel->reason_code;
    config_.change_commentary = crSel->commentary;

    updateConfigFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving IR curve bootstrap config: "
                              << boost::uuids::to_string(config_.id);

    QPointer<IrCurveBootstrapConfigDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, config = config_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_ir_curve_bootstrap_config_request request;
        request.data = config;
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
                    BOOST_LOG_SEV(lg(), info) << "IR Curve Bootstrap Config saved successfully";
                    QString code =
                        QString::fromStdString(boost::uuids::to_string(self->config_.id));
                    self->hasChanges_ = false;
                    self->updateSaveButtonState();
                    emit self->configSaved(code);
                    self->notifySaveSuccess(tr("IR Curve Bootstrap Config '%1' saved").arg(code));
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

void IrCurveBootstrapConfigDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this,
            "Disconnected",
            "Cannot delete IR curve bootstrap config while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(boost::uuids::to_string(config_.id));
    auto reply = MessageBoxHelper::question(
        this,
        "Delete IR Curve Bootstrap Config",
        QString("Are you sure you want to delete IR curve bootstrap config '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting IR curve bootstrap config: "
                              << boost::uuids::to_string(config_.id);

    QPointer<IrCurveBootstrapConfigDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, id_str = boost::uuids::to_string(config_.id)]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_ir_curve_bootstrap_config_request request;
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
            BOOST_LOG_SEV(lg(), info) << "IR Curve Bootstrap Config deleted successfully";
            emit self->statusMessage(QString("IR Curve Bootstrap Config '%1' deleted").arg(code));
            emit self->configDeleted(code);
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
