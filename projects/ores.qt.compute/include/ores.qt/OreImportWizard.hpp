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
#ifndef ORES_QT_ORE_IMPORT_WIZARD_HPP
#define ORES_QT_ORE_IMPORT_WIZARD_HPP

#include <map>
#include <optional>
#include <string>
#include <boost/uuid/uuid.hpp>
#include <QWizard>
#include <QWizardPage>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QProgressBar>
#include <QTextEdit>
#include <QRadioButton>
#include <QCheckBox>
#include <QListWidget>
#include <QTreeWidget>
#include <QComboBox>
#include <QDateEdit>
#include <filesystem>
#include <vector>
#include "ores.logging/make_logger.hpp"
#include "ores.qt/ClientManager.hpp"
#include "ores.ore/scanner/scan_result.hpp"
#include "ores.ore/planner/import_choices.hpp"
#include "ores.ore/planner/ore_import_plan.hpp"
#include "ores.refdata.api/domain/book.hpp"
#include "ores.ore.api/messaging/ore_import_protocol.hpp"

namespace ores::qt {

/**
 * @brief 7-page wizard for importing ORE directory data into OreStudio.
 *
 * Pages:
 * 1. Welcome      - static explanation
 * 2. Directory    - folder picker; async scan on Next
 * 3. Scan Summary - counts + editable exclusion list; live hierarchy preview
 * 4. Currency     - All vs Missing-only toggle; shows currency count
 * 5. Portfolio    - parent portfolio name / create toggle
 * 6. Trade        - date/lifecycle/counterparty defaults; executes import
 * 7. Done         - summary counts and per-step results
 */
class OreImportWizard final : public QWizard {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_import_wizard";

    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    enum PageId {
        Page_Welcome = 0,
        Page_Directory,
        Page_ScanSummary,
        Page_Currency,
        Page_Portfolio,
        Page_TradeImport,
        Page_Done
    };

    explicit OreImportWizard(ClientManager* clientManager,
                             std::optional<boost::uuids::uuid> targetBookId = std::nullopt,
                             const std::string& targetBookName = "",
                             QWidget* parent = nullptr);

    const std::string& targetBookName() const { return targetBookName_; }
    ~OreImportWizard() override = default;

    ClientManager* clientManager() const { return clientManager_; }

    // Shared state — written by pages, read by subsequent pages
    ore::scanner::scan_result& scanResult() { return scanResult_; }
    ore::planner::import_choices& choices() { return choices_; }
    ore::planner::ore_import_plan& importPlan() { return importPlan_; }

    void setScanResult(ore::scanner::scan_result r) { scanResult_ = std::move(r); }

    // ORE directory picked on Page_Directory — needed by Page_TradeImport for upload
    void setOreDir(std::filesystem::path dir) { ore_dir_ = std::move(dir); }
    const std::filesystem::path& oreDir() const { return ore_dir_; }

    // HTTP base URL set by OreImportController before the wizard is shown
    void setHttpBaseUrl(std::string url) { http_base_url_ = std::move(url); }
    const std::string& httpBaseUrl() const { return http_base_url_; }

    // Results set by Page_TradeImport after the server-side import completes
    void setImportResponse(ores::ore::messaging::ore_import_response response) {
        import_response_ = std::move(response);
        importSuccess_ = import_response_.success;
        importError_ = QString::fromStdString(import_response_.message);
    }
    bool importSuccess() const { return importSuccess_; }
    QString importError() const { return importError_; }
    const ores::ore::messaging::ore_import_response& importResponse() const {
        return import_response_;
    }

    // Existing ISO codes fetched on Page_Currency
    const std::set<std::string>& existingIsoCodes() const { return existingIsoCodes_; }
    void setExistingIsoCodes(std::set<std::string> codes) {
        existingIsoCodes_ = std::move(codes);
    }

    // Existing portfolio names fetched on Page_Portfolio
    const std::vector<std::string>& existingPortfolioNames() const {
        return existingPortfolioNames_;
    }
    void setExistingPortfolioNames(std::vector<std::string> names) {
        existingPortfolioNames_ = std::move(names);
    }

private:
    void setupPages();

    ClientManager* clientManager_;
    std::string targetBookName_;
    ore::scanner::scan_result scanResult_;
    ore::planner::import_choices choices_;
    ore::planner::ore_import_plan importPlan_;
    std::set<std::string> existingIsoCodes_;
    std::vector<std::string> existingPortfolioNames_;

    std::filesystem::path ore_dir_;
    std::string http_base_url_;
    ores::ore::messaging::ore_import_response import_response_;
    bool importSuccess_ = false;
    QString importError_;
};

// ============================================================================
// Page declarations
// ============================================================================

class OreWelcomePage final : public QWizardPage {
    Q_OBJECT
public:
    explicit OreWelcomePage(OreImportWizard* wizard);
private:
    OreImportWizard* wizard_;
};

class OreDirectoryPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_directory_page";
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OreDirectoryPage(OreImportWizard* wizard);
    bool isComplete() const override;
    bool validatePage() override;

private slots:
    void onBrowseClicked();
    void onScanFinished();

private:
    void startScan();

    OreImportWizard* wizard_;
    QLineEdit* dirEdit_;
    QPushButton* browseBtn_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    bool scanComplete_ = false;
    bool scanning_ = false;
};

class OreScanSummaryPage final : public QWizardPage {
    Q_OBJECT
public:
    explicit OreScanSummaryPage(OreImportWizard* wizard);
    void initializePage() override;

private slots:
    void onExclusionAdded();
    void onExclusionRemoved();

private:
    void refreshSummary();

    OreImportWizard* wizard_;
    QLabel* summaryLabel_;
    QListWidget* exclusionList_;
    QLineEdit* exclusionEdit_;
    QPushButton* addBtn_;
    QPushButton* removeBtn_;
    QLabel* hierarchyLabel_;
};

class OreCurrencyPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_currency_page";
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OreCurrencyPage(OreImportWizard* wizard);
    void initializePage() override;

private slots:
    void onFetchFinished();
    void onModeChanged();

private:
    OreImportWizard* wizard_;
    QRadioButton* allRadio_;
    QRadioButton* missingRadio_;
    QLabel* statusLabel_;
    QLabel* countLabel_;
    bool fetchDone_ = false;
};

class OrePortfolioPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_portfolio_page";
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OrePortfolioPage(OreImportWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;
    bool validatePage() override;

private slots:
    void onBookSelectionChanged();
    void onBooksFetchFinished();

private:
    OreImportWizard* wizard_;
    QComboBox* parentCombo_;
    QLabel* hierarchyPreviewLabel_;
    bool fetchDone_ = false;
    // name → book domain object for existing books fetched from server
    std::map<std::string, refdata::domain::book> booksByName_;
};

class OreTradeImportPage final : public QWizardPage {
    Q_OBJECT

private:
    inline static std::string_view logger_name = "ores.qt.ore_trade_import_page";
    [[nodiscard]] static auto& lg() {
        using namespace ores::logging;
        static auto instance = make_logger(logger_name);
        return instance;
    }

public:
    explicit OreTradeImportPage(OreImportWizard* wizard);
    void initializePage() override;
    bool isComplete() const override;
    bool validatePage() override;

private slots:
    void onActivityTypesFetchFinished();
    void onCounterpartiesFetchFinished();
    void onImportFinished();

private:
    void startImport();
    void appendLog(const QString& msg);

    OreImportWizard* wizard_;
    QLineEdit* tradeDateEdit_;
    QComboBox* lifecycleEventCombo_;
    QComboBox* defaultCounterpartyCombo_;
    QLabel* counterpartyStatusLabel_;
    QLabel* statusLabel_;
    QProgressBar* progressBar_;
    QTextEdit* logOutput_;
    bool importDone_ = false;
    bool importStarted_ = false;
};

class OreDonePage final : public QWizardPage {
    Q_OBJECT
public:
    explicit OreDonePage(OreImportWizard* wizard);
    void initializePage() override;
private:
    OreImportWizard* wizard_;
    QLabel* summaryLabel_;
};

}

#endif
