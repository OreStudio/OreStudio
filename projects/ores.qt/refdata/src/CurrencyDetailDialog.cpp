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
#include "ores.qt/CurrencyDetailDialog.hpp"
#include "ores.qt/BadgeComboHelper.hpp"
#include "ores.qt/ChangeReasonDialog.hpp"
#include "ores.qt/DynamicComboSetup.hpp"
#include "ores.qt/IconUtils.hpp"
#include "ores.qt/ImageCache.hpp"
#include "ores.qt/LookupFetcher.hpp"
#include "ores.qt/MessageBoxHelper.hpp"
#include "ores.qt/WidgetUtils.hpp"
#include "ores.refdata.api/messaging/currency_protocol.hpp"
#include "ui_CurrencyDetailDialog.h"
#include <QComboBox>
#include <QFutureWatcher>
#include <QIcon>
#include <QLineEdit>
#include <QMessageBox>
#include <QtConcurrent>

namespace ores::qt {

using namespace ores::logging;

CurrencyDetailDialog::CurrencyDetailDialog(QWidget* parent)
    : DetailDialogBase(parent)
    , ui_(new Ui::CurrencyDetailDialog)
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

CurrencyDetailDialog::~CurrencyDetailDialog() {
    delete ui_;
}

QTabWidget* CurrencyDetailDialog::tabWidget() const {
    return ui_->tabWidget;
}

QWidget* CurrencyDetailDialog::provenanceTab() const {
    return ui_->provenanceTab;
}

ProvenanceWidget* CurrencyDetailDialog::provenanceWidget() const {
    return ui_->provenanceWidget;
}

QString CurrencyDetailDialog::code() const {
    return QString::fromStdString(currency_.iso_code);
}

void CurrencyDetailDialog::setupUi() {
    ui_->saveButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Save, IconUtils::DefaultIconColor));
    ui_->saveButton->setEnabled(false);

    ui_->deleteButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Delete, IconUtils::DefaultIconColor));

    ui_->closeButton->setIcon(
        IconUtils::createRecoloredIcon(Icon::Dismiss, IconUtils::DefaultIconColor));

    // Flag editor hosted in the .ui flagGroup; base class owns the button
    // (also wires the inline key-field icon — see initKeyFlagField()).
    initFlagButton(ui_->flagGroup->layout());

    toolBar_ = new QToolBar(this);
    toolBar_->setMovable(false);
    toolBar_->setFloatable(false);

    revertAction_ = new QAction(tr("Revert"), this);
    revertAction_->setIcon(IconUtils::createRecoloredIcon(Icon::ArrowRotateCounterclockwise,
                                                          IconUtils::DefaultIconColor));
    revertAction_->setToolTip(tr("Revert currency to this historical version"));
    connect(revertAction_, &QAction::triggered, this, &CurrencyDetailDialog::onRevertClicked);
    toolBar_->addAction(revertAction_);
    revertAction_->setVisible(false);

    toolBar_->addSeparator();

    firstVersionAction_ = new QAction(tr("First"), this);
    firstVersionAction_->setIcon(
        IconUtils::createRecoloredIcon(Icon::ArrowPrevious, IconUtils::DefaultIconColor));
    firstVersionAction_->setToolTip(tr("First version"));
    connect(firstVersionAction_,
            &QAction::triggered,
            this,
            &CurrencyDetailDialog::onFirstVersionClicked);
    toolBar_->addAction(firstVersionAction_);
    firstVersionAction_->setVisible(false);

    prevVersionAction_ = new QAction(tr("Previous"), this);
    prevVersionAction_->setIcon(
        IconUtils::createRecoloredIcon(Icon::ArrowLeft, IconUtils::DefaultIconColor));
    prevVersionAction_->setToolTip(tr("Previous version"));
    connect(
        prevVersionAction_, &QAction::triggered, this, &CurrencyDetailDialog::onPrevVersionClicked);
    toolBar_->addAction(prevVersionAction_);
    prevVersionAction_->setVisible(false);

    nextVersionAction_ = new QAction(tr("Next"), this);
    nextVersionAction_->setIcon(
        IconUtils::createRecoloredIcon(Icon::ArrowRight, IconUtils::DefaultIconColor));
    nextVersionAction_->setToolTip(tr("Next version"));
    connect(
        nextVersionAction_, &QAction::triggered, this, &CurrencyDetailDialog::onNextVersionClicked);
    toolBar_->addAction(nextVersionAction_);
    nextVersionAction_->setVisible(false);

    lastVersionAction_ = new QAction(tr("Last"), this);
    lastVersionAction_->setIcon(
        IconUtils::createRecoloredIcon(Icon::ArrowNext, IconUtils::DefaultIconColor));
    lastVersionAction_->setToolTip(tr("Last version"));
    connect(
        lastVersionAction_, &QAction::triggered, this, &CurrencyDetailDialog::onLastVersionClicked);
    toolBar_->addAction(lastVersionAction_);
    lastVersionAction_->setVisible(false);

    if (auto* mainLayout = qobject_cast<QVBoxLayout*>(layout()))
        mainLayout->insertWidget(0, toolBar_);
}

std::optional<boost::uuids::uuid> CurrencyDetailDialog::entityImageId() const {
    return currency_.image_id;
}

QLineEdit* CurrencyDetailDialog::keyFlagField() const {
    return ui_->isoCodeEdit;
}

QIcon CurrencyDetailDialog::keyFlagIcon(const std::string& key) const {
    return imageCache() ? imageCache()->getCurrencyFlagIcon(key) : QIcon();
}

void CurrencyDetailDialog::setupCombos() {}

void CurrencyDetailDialog::setupConnections() {
    connect(ui_->saveButton, &QPushButton::clicked, this, &CurrencyDetailDialog::onSaveClicked);
    connect(ui_->deleteButton, &QPushButton::clicked, this, &CurrencyDetailDialog::onDeleteClicked);
    connect(ui_->closeButton, &QPushButton::clicked, this, &CurrencyDetailDialog::onCloseClicked);
    connect(this, &DetailDialogBase::flagEdited, this, &CurrencyDetailDialog::onFieldChanged);

    connect(ui_->isoCodeEdit, &QLineEdit::textChanged, this, &CurrencyDetailDialog::onCodeChanged);
    connect(ui_->nameEdit, &QLineEdit::textChanged, this, &CurrencyDetailDialog::onFieldChanged);
    connect(
        ui_->numericCodeEdit, &QLineEdit::textChanged, this, &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->monetaryNatureCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->marketTierCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->symbolEdit, &QLineEdit::textChanged, this, &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->fractionSymbolEdit,
            &QLineEdit::textChanged,
            this,
            &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->formatEdit, &QLineEdit::textChanged, this, &CurrencyDetailDialog::onFieldChanged);
    connect(ui_->roundingTypeCombo,
            &QComboBox::currentIndexChanged,
            this,
            &CurrencyDetailDialog::onFieldChanged);
}

void CurrencyDetailDialog::setClientManager(ClientManager* clientManager) {
    clientManager_ = clientManager;
    populateMonetaryNatureCombo();
    setup_badge_combo(this, ui_->monetaryNatureCombo, badgeCache(), "monetary_nature");
    populateMarketTierCombo();
    setup_badge_combo(this, ui_->marketTierCombo, badgeCache(), "currency_market_tier");
    populateRoundingTypeCombo();
}

void CurrencyDetailDialog::setUsername(const std::string& username) {
    username_ = username;
}

void CurrencyDetailDialog::setCurrency(const refdata::domain::currency& currency) {
    currency_ = currency;
    updateUiFromCurrency();
}

void CurrencyDetailDialog::setCreateMode(bool createMode) {
    createMode_ = createMode;
    ui_->isoCodeEdit->setReadOnly(!createMode);
    ui_->deleteButton->setVisible(!createMode);
    setProvenanceEnabled(!createMode);
    hasChanges_ = false;
    updateSaveButtonState();
}

void CurrencyDetailDialog::markDirty() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyDetailDialog::setReadOnly(bool readOnly, int versionNumber) {
    historicalVersion_ = versionNumber;
    readOnly_ = readOnly;
    ui_->isoCodeEdit->setReadOnly(true);
    ui_->nameEdit->setReadOnly(readOnly);
    ui_->numericCodeEdit->setReadOnly(readOnly);
    ui_->monetaryNatureCombo->setEnabled(!readOnly);
    ui_->marketTierCombo->setEnabled(!readOnly);
    ui_->symbolEdit->setReadOnly(readOnly);
    ui_->fractionSymbolEdit->setReadOnly(readOnly);
    ui_->formatEdit->setReadOnly(readOnly);
    ui_->roundingTypeCombo->setEnabled(!readOnly);
    ui_->saveButton->setVisible(!readOnly);
    ui_->deleteButton->setVisible(!readOnly);
    if (revertAction_)
        revertAction_->setVisible(readOnly);
}

void CurrencyDetailDialog::setHistory(const std::vector<refdata::domain::currency>& history,
                                      int versionNumber) {
    history_ = history;

    // Find index of the requested version (history is newest-first)
    currentHistoryIndex_ = 0;
    for (size_t i = 0; i < history_.size(); ++i) {
        if (history_[i].version == versionNumber) {
            currentHistoryIndex_ = static_cast<int>(i);
            break;
        }
    }

    displayCurrentVersion();
    showVersionNavActions(true);
}

void CurrencyDetailDialog::displayCurrentVersion() {
    if (history_.empty() || currentHistoryIndex_ < 0 ||
        currentHistoryIndex_ >= static_cast<int>(history_.size())) {
        return;
    }

    const auto& version = history_[currentHistoryIndex_];
    setCurrency(version);
    setReadOnly(true, version.version);
    updateVersionNavButtonStates();
}

void CurrencyDetailDialog::updateVersionNavButtonStates() {
    if (history_.empty()) {
        showVersionNavActions(false);
        return;
    }

    bool atOldest = (currentHistoryIndex_ == static_cast<int>(history_.size()) - 1);
    bool atNewest = (currentHistoryIndex_ == 0);

    if (firstVersionAction_)
        firstVersionAction_->setEnabled(!atOldest); // Go to oldest
    if (prevVersionAction_)
        prevVersionAction_->setEnabled(!atOldest); // Go to older
    if (nextVersionAction_)
        nextVersionAction_->setEnabled(!atNewest); // Go to newer
    if (lastVersionAction_)
        lastVersionAction_->setEnabled(!atNewest); // Go to latest
}

void CurrencyDetailDialog::showVersionNavActions(bool visible) {
    if (firstVersionAction_)
        firstVersionAction_->setVisible(visible);
    if (prevVersionAction_)
        prevVersionAction_->setVisible(visible);
    if (nextVersionAction_)
        nextVersionAction_->setVisible(visible);
    if (lastVersionAction_)
        lastVersionAction_->setVisible(visible);
}

void CurrencyDetailDialog::onFirstVersionClicked() {
    if (history_.empty())
        return;
    currentHistoryIndex_ = static_cast<int>(history_.size()) - 1;
    displayCurrentVersion();
}

void CurrencyDetailDialog::onPrevVersionClicked() {
    if (history_.empty())
        return;
    if (currentHistoryIndex_ < static_cast<int>(history_.size()) - 1) {
        ++currentHistoryIndex_;
        displayCurrentVersion();
    }
}

void CurrencyDetailDialog::onNextVersionClicked() {
    if (history_.empty())
        return;
    if (currentHistoryIndex_ > 0) {
        --currentHistoryIndex_;
        displayCurrentVersion();
    }
}

void CurrencyDetailDialog::onLastVersionClicked() {
    if (history_.empty())
        return;
    currentHistoryIndex_ = 0;
    displayCurrentVersion();
}

void CurrencyDetailDialog::onRevertClicked() {
    auto reply = MessageBoxHelper::question(
        this,
        tr("Revert Currency"),
        tr("Are you sure you want to revert '%1' to version %2?\n\n"
           "This will create a new version with the data from version %2.")
            .arg(code())
            .arg(historicalVersion_),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes)
        return;

    emit revertRequested(currency_);
}

void CurrencyDetailDialog::populateMonetaryNatureCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating monetary_nature combo";
    populateDynamicCombo<refdata::domain::monetary_nature>(
        ui_->monetaryNatureCombo,
        this,
        clientManager_,
        &fetch_monetary_natures,
        "monetaryNatureWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(currency_.monetary_nature); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load monetary natures: %1").arg(error));
        },
        [this]() {
            setup_badge_combo(this, ui_->monetaryNatureCombo, badgeCache(), "monetary_nature");
        },
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; });
}
void CurrencyDetailDialog::populateMarketTierCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating market_tier combo";
    populateDynamicCombo<refdata::domain::currency_market_tier>(
        ui_->marketTierCombo,
        this,
        clientManager_,
        &fetch_currency_market_tiers,
        "marketTierWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(currency_.market_tier); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load currency market tiers: %1").arg(error));
        },
        [this]() {
            setup_badge_combo(this, ui_->marketTierCombo, badgeCache(), "currency_market_tier");
        },
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; });
}
void CurrencyDetailDialog::populateRoundingTypeCombo() {
    BOOST_LOG_SEV(lg(), debug) << "Populating rounding_type combo";
    populateDynamicCombo<refdata::domain::rounding_type>(
        ui_->roundingTypeCombo,
        this,
        clientManager_,
        &fetch_rounding_types,
        "roundingTypeWatcher",
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto& t) { return QString::fromStdString(t.description); },
        [](const auto& t) { return t.display_order; },
        [this]() { return QString::fromStdString(currency_.rounding_type); },
        [this](const QString& error) {
            emit errorMessage(tr("Failed to load rounding types: %1").arg(error));
        },
        []() {},
        QObject::tr("Loading…"),
        QObject::tr("Failed to load"),
        [](const auto& t) { return QString::fromStdString(t.code); },
        [](const auto&) { return false; });
}
void CurrencyDetailDialog::updateUiFromCurrency() {
    ui_->isoCodeEdit->setText(QString::fromStdString(currency_.iso_code));
    ui_->nameEdit->setText(QString::fromStdString(currency_.name));
    ui_->numericCodeEdit->setText(QString::fromStdString(currency_.numeric_code));
    {
        const auto val = QString::fromStdString(currency_.monetary_nature);
        const int idx = ui_->monetaryNatureCombo->findData(val);
        if (idx >= 0)
            ui_->monetaryNatureCombo->setCurrentIndex(idx);
    }
    {
        const auto val = QString::fromStdString(currency_.market_tier);
        const int idx = ui_->marketTierCombo->findData(val);
        if (idx >= 0)
            ui_->marketTierCombo->setCurrentIndex(idx);
    }
    ui_->symbolEdit->setText(QString::fromStdString(currency_.symbol));
    ui_->fractionSymbolEdit->setText(QString::fromStdString(currency_.fraction_symbol));
    ui_->fractionsPerUnitSpinBox->setValue(currency_.fractions_per_unit);
    ui_->formatEdit->setText(QString::fromStdString(currency_.format));
    {
        const auto val = QString::fromStdString(currency_.rounding_type);
        const int idx = ui_->roundingTypeCombo->findData(val);
        if (idx >= 0)
            ui_->roundingTypeCombo->setCurrentIndex(idx);
    }
    ui_->roundingPrecisionSpinBox->setValue(currency_.rounding_precision);

    populateProvenance(currency_.version,
                       currency_.modified_by,
                       currency_.performed_by,
                       currency_.recorded_at,
                       currency_.change_reason_code,
                       currency_.change_commentary);

    hasChanges_ = false;
    updateSaveButtonState();
}

void CurrencyDetailDialog::updateCurrencyFromUi() {
    if (createMode_) {
        currency_.iso_code = ui_->isoCodeEdit->text().trimmed().toStdString();
    }
    currency_.name = ui_->nameEdit->text().trimmed().toStdString();
    currency_.numeric_code = ui_->numericCodeEdit->text().trimmed().toStdString();
    currency_.monetary_nature = ui_->monetaryNatureCombo->currentText().toStdString();
    currency_.market_tier = ui_->marketTierCombo->currentText().toStdString();
    currency_.symbol = ui_->symbolEdit->text().trimmed().toStdString();
    currency_.fraction_symbol = ui_->fractionSymbolEdit->text().trimmed().toStdString();
    currency_.fractions_per_unit = ui_->fractionsPerUnitSpinBox->value();
    currency_.format = ui_->formatEdit->text().trimmed().toStdString();
    currency_.rounding_type = ui_->roundingTypeCombo->currentText().toStdString();
    currency_.rounding_precision = ui_->roundingPrecisionSpinBox->value();
    currency_.modified_by = username_;
}

void CurrencyDetailDialog::onCodeChanged(const QString& /* text */) {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyDetailDialog::onFieldChanged() {
    hasChanges_ = true;
    updateSaveButtonState();
}

void CurrencyDetailDialog::updateSaveButtonState() {
    bool canSave = hasChanges_ && validateInput() && !readOnly_;
    ui_->saveButton->setEnabled(canSave);
}

bool CurrencyDetailDialog::validateInput() {
    const QString iso_code_val = ui_->isoCodeEdit->text().trimmed();
    const QString name_val = ui_->nameEdit->text().trimmed();
    const QString numeric_code_val = ui_->numericCodeEdit->text().trimmed();
    const QString symbol_val = ui_->symbolEdit->text().trimmed();

    return true && !iso_code_val.isEmpty() && !name_val.isEmpty() && !numeric_code_val.isEmpty() &&
           !symbol_val.isEmpty();
}

void CurrencyDetailDialog::onSaveClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot save currency while disconnected from server.");
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
    currency_.change_reason_code = crSel->reason_code;
    currency_.change_commentary = crSel->commentary;
    if (flagChanged())
        currency_.image_id = selectedImageId();

    updateCurrencyFromUi();

    BOOST_LOG_SEV(lg(), info) << "Saving currency: " << currency_.iso_code;

    QPointer<CurrencyDetailDialog> self = this;

    struct SaveResult {
        bool success;
        std::string message;
    };

    auto task = [self, currency = currency_]() -> SaveResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::save_currency_request request;
        request.data = currency;
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
            BOOST_LOG_SEV(lg(), info) << "Currency saved successfully";
            QString code = QString::fromStdString(self->currency_.iso_code);
            self->hasChanges_ = false;
            self->resetFlagChanged();
            self->updateSaveButtonState();
            emit self->currencySaved(code);
            self->notifySaveSuccess(tr("Currency '%1' saved").arg(code));
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

void CurrencyDetailDialog::onDeleteClicked() {
    if (!clientManager_ || !clientManager_->isConnected()) {
        MessageBoxHelper::warning(
            this, "Disconnected", "Cannot delete currency while disconnected from server.");
        return;
    }

    QString code = QString::fromStdString(currency_.iso_code);
    auto reply = MessageBoxHelper::question(
        this,
        "Delete Currency",
        QString("Are you sure you want to delete currency '%1'?").arg(code),
        QMessageBox::Yes | QMessageBox::No);

    if (reply != QMessageBox::Yes) {
        return;
    }

    const auto crSel =
        promptChangeReason(ChangeReasonDialog::OperationType::Delete, false, "common");
    if (!crSel)
        return;

    BOOST_LOG_SEV(lg(), info) << "Deleting currency: " << currency_.iso_code;

    QPointer<CurrencyDetailDialog> self = this;

    struct DeleteResult {
        bool success;
        std::string message;
    };

    auto task = [self, code = currency_.iso_code]() -> DeleteResult {
        if (!self || !self->clientManager_) {
            return {false, "Dialog closed"};
        }

        refdata::messaging::delete_currency_request request;
        request.iso_codes = {code};
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
            BOOST_LOG_SEV(lg(), info) << "Currency deleted successfully";
            emit self->statusMessage(QString("Currency '%1' deleted").arg(code));
            emit self->currencyDeleted(code);
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
