declare module 'react-plotly.js' {
  import { ComponentType } from 'react';
  
  interface PlotParams {
    data: any[];
    layout?: any;
    config?: any;
    style?: React.CSSProperties;
    className?: string;
    onInitialized?: (figure: any, graphDiv: HTMLElement) => void;
    onUpdate?: (figure: any, graphDiv: HTMLElement) => void;
    onPurge?: (figure: any, graphDiv: HTMLElement) => void;
    onError?: (err: any) => void;
    debug?: boolean;
    onSelected?: (eventData: any) => void;
    onDeselected?: () => void;
    onHover?: (eventData: any) => void;
    onUnhover?: (eventData: any) => void;
    onClick?: (eventData: any) => void;
    onClickAnnotation?: (eventData: any) => void;
    onBeforeHover?: (eventData: any) => void;
    onAfterExport?: () => void;
    onAfterPlot?: () => void;
    onAnimated?: () => void;
    onAnimatingFrame?: (eventData: any) => void;
    onAnimationInterrupted?: () => void;
    onAutoSize?: () => void;
    onBeforeExport?: () => void;
    onButtonClicked?: (eventData: any) => void;
    onDoubleClick?: () => void;
    onFramework?: () => void;
    onLegendClick?: (eventData: any) => boolean;
    onLegendDoubleClick?: (eventData: any) => boolean;
    onRelayout?: (eventData: any) => void;
    onRedraw?: () => void;
    onRestyle?: (eventData: any) => void;
    onSliderChange?: (eventData: any) => void;
    onSliderEnd?: (eventData: any) => void;
    onSliderStart?: (eventData: any) => void;
    onTransitioning?: () => void;
    onTransitionInterrupted?: () => void;
    onWebGlContextLost?: () => void;
  }
  
  declare const Plot: ComponentType<PlotParams>;
  export default Plot;
}